module ArrowPlayground.EventMachine where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Cochoice (class Cochoice, unleft)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)

foreign import data EventEmitter :: Type -> Type

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

fixEs :: forall a. (EventSource a -> EventSource a) -> EventSource a
fixEs f = EventSource \handler -> do
  let eventSource = f (EventSource \handler' -> subscribe eventSource handler')
  subscribe eventSource handler

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

-- instance Cochoice EventMachine where
--   unleft (EventMachine f) = EventMachine \eventSource ->
--     catMaybes $ f $ map (either Just (const Nothing)) eventSource
--   unright (EventMachine f) = EventMachine \eventSource ->
--     catMaybes $ f $ map (either (const Nothing) Just) eventSource
