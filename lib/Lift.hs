module Lift where

import Prelude hiding (Applicative, liftA2, pure, (<*>))

class Functor t => Applicative t where
    pure :: a -> t a
    (<*>) :: t (a -> b) -> t a -> t b

infixl 4 <*>

instance Applicative ((->) r) where
    pure = const
    (<*>) p f x = p x (f x)

liftA0 ::  a                      ->  r -> a
liftA1 :: (a -> b               ) -> (r -> a) ->  r -> b
liftA2 :: (a -> b -> c          ) -> (r -> a) -> (r -> b) ->  r -> c
liftA3 :: (a -> b -> c -> d     ) -> (r -> a) -> (r -> b) -> (r -> c) ->  r -> d
liftA4 :: (a -> b -> c -> d -> e) -> (r -> a) -> (r -> b) -> (r -> c) -> (r -> d) -> r -> e

liftA0 v         = pure v
liftA1 u f       = pure u <*> f
liftA2 b f g     = pure b <*> f <*> g
liftA3 t f g h   = pure t <*> f <*> g <*> h
liftA4 q f g h i = pure q <*> f <*> g <*> h <*> i
