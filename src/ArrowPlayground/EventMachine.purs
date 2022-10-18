module ArrowPlayground.EventMachine where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Cochoice (class Cochoice)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Timer (setTimeout)

foreign import data EventEmitter :: Type -> Type

foreign import newEventEmitter :: forall a. Effect (EventEmitter a)
foreign import emit :: forall a. EventEmitter a -> a -> Effect Unit
foreign import listen :: forall a. EventEmitter a -> (a -> Effect Unit) -> Effect Unit

newtype EventSource a = EventSource ((a -> Effect Unit) -> Effect Unit)

instance Functor EventSource where
  map f (EventSource g) = EventSource \cb -> g (cb <<< f)

instance Apply EventSource where
  apply (EventSource f) (EventSource g) = EventSource \cb -> f \f' -> g (cb <<< f')

instance Applicative EventSource where
  pure a = EventSource \cb -> cb a

instance Bind EventSource where
  bind (EventSource f) k = EventSource \cb -> f \a -> case k a of EventSource g -> g cb

instance Monad EventSource

listenEventEmitter :: forall a. EventEmitter a -> EventSource a
listenEventEmitter ee = EventSource \cb -> listen ee cb

subscribe :: forall a. EventSource a -> (a -> Effect Unit) -> Effect Unit
subscribe (EventSource f) = f

separate :: forall a b. EventSource (Either a b) -> Tuple (EventSource a) (EventSource b)
separate eventSource = Tuple (EventSource subscribeA) (EventSource subscribeB)
  where
  subscribeA :: (a -> Effect Unit) -> Effect Unit
  subscribeA handler = subscribe eventSource (either handler (const (pure unit)))

  subscribeB :: (b -> Effect Unit) -> Effect Unit
  subscribeB handler = subscribe eventSource (either (const (pure unit)) handler)

catMaybes :: forall a. EventSource (Maybe a) -> EventSource a
catMaybes eventSource = snd $ separate $ map (\a -> maybe (Left unit) Right a) eventSource

filter :: forall a. (a -> Boolean) -> EventSource a -> EventSource a
filter p eventSource = catMaybes $ map (\a -> if p a then Just a else Nothing) eventSource

merge :: forall a b. EventSource a -> EventSource b -> EventSource (Either a b)
merge eventSourceA eventSourceB = EventSource \handler -> do
  subscribe eventSourceA (handler <<< Left)
  subscribe eventSourceB (handler <<< Right)

concat :: forall a. EventSource (Either a a) -> EventSource a
concat eventSource = EventSource \handler -> subscribe eventSource (either handler handler)

dup :: forall a. EventSource a -> EventSource (Either a a)
dup eventSource = merge eventSource eventSource

delay :: forall a. Int -> EventSource a -> EventSource a
delay ms eventSource = EventSource \handler -> do
  ee <- newEventEmitter
  subscribe eventSource \a -> do
    void $ setTimeout ms (emit ee a)
  listen ee handler

rec :: forall a. (EventSource a -> EventSource a) -> EventSource a
rec f = EventSource \handler -> do
  ee <- newEventEmitter
  let eventSource = f (listenEventEmitter ee)
  subscribe eventSource handler
  subscribe eventSource (emit ee)

newtype EventMachine a b = EventMachine (EventSource a -> EventSource b)

instance Semigroupoid EventMachine where
  compose (EventMachine f) (EventMachine g) = EventMachine (f <<< g)

instance Category EventMachine where
  identity = EventMachine identity

instance Functor (EventMachine a) where
  map f (EventMachine g) = EventMachine (map f <<< g)

instance Profunctor EventMachine where
  dimap f g (EventMachine h) = EventMachine (map g <<< h <<< map f)

instance Choice EventMachine where
  left (EventMachine f) = EventMachine \eventSource ->
    merge (f $ fst $ separate eventSource) $ snd $ separate eventSource
  right (EventMachine f) = EventMachine \eventSource ->
    merge (fst $ separate eventSource) $ f $ snd $ separate eventSource

instance Cochoice EventMachine where
  unleft (EventMachine f) = EventMachine \eventSourceA ->
    let
      eventSourceBC = rec \ev -> f $ merge (eventSourceA) $ snd $ separate ev
    in
      fst $ separate $ eventSourceBC
  unright (EventMachine f) = EventMachine \eventSourceB ->
    let
      eventSourceAC = rec \ev -> f $ merge (fst $ separate ev) $ eventSourceB
    in
      snd $ separate $ eventSourceAC
