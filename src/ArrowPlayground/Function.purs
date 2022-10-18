module ArrowPlayground.Function where

import Prelude

import Control.Lazy (class Lazy, defer)
import Data.Tuple (Tuple(..), fst, snd)

-- Arrow reccursion
-- Costrong class does not work because of the lack of lazy evaluation

newtype Func a b = Func (a -> b)

-- | Not work
-- | Tuple b c = f $ Tuple a c

{-
instance Costrong Func where
  unfirst (Func f) a = let
    Tuple b c = f $ Tuple a c
    in b
  unsecond (Func f) a = let
    Tuple c b = f $ Tuple c a
    in b
-}

class CostrongLazy p where
  unfirstLazy :: forall a b c. Lazy c => p (Tuple a c) (Tuple b c) -> p a b
  unsecondLazy :: forall a b c. Lazy a => p (Tuple a b) (Tuple a c) -> p b c

instance CostrongLazy Function where
  unfirstLazy f a = fst $ f $ Tuple a $ defer \_ -> go
    where
    go = snd $ f $ Tuple a $ defer \_ -> go
  unsecondLazy f a = snd $ f $ Tuple (defer \_ -> go) a
    where
    go = fst $ f $ Tuple (defer \_ -> go) a

-- | factorial in arrow
factorialLoop :: Tuple Int (Int -> Int) -> Tuple (Unit -> Int) (Int -> Int)
factorialLoop (Tuple n f) = Tuple (\_ -> f n) \k -> if k == 0 then 1 else k * f (k - 1)

eval :: (Unit -> Int) -> Int
eval f = f unit

factorial :: Int -> Int
factorial = unfirstLazy factorialLoop >>> eval