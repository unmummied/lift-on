module Lift where

import Prelude hiding (Applicative, liftA2, pure, (<*>))

class Functor t => Applicative t where
    pure  :: a -> t a
    (<*>) :: t (a -> b) -> t a -> t b

infixl 4 <*>

instance Applicative ((->) r) where
    pure        = const
    (<*>) p f x = p x (f x)

liftA0 ::                      z                                                  -> r -> z
liftA1 :: (a                -> z) -> (r -> a)                                     -> r -> z
liftA2 :: (a -> b           -> z) -> (r -> a) -> (r -> b)                         -> r -> z
liftA3 :: (a -> b -> c      -> z) -> (r -> a) -> (r -> b) -> (r -> c)             -> r -> z
liftA4 :: (a -> b -> c -> d -> z) -> (r -> a) -> (r -> b) -> (r -> c) -> (r -> d) -> r -> z
liftA0 v         = pure v
liftA1 u f       = pure u <*> f
liftA2 b f g     = pure b <*> f <*> g
liftA3 t f g h   = pure t <*> f <*> g <*> h
liftA4 q f g h i = pure q <*> f <*> g <*> h <*> i
