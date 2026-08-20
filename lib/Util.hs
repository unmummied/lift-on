module Util where

flip0 :: (a                          -> z) -> a                          -> z
flip1 :: (a -> b                     -> z) -> b -> a                     -> z
flip2 :: (a -> b -> c                -> z) -> c -> a -> b                -> z
flip3 :: (a -> b -> c -> d           -> z) -> d -> a -> b -> c           -> z
flip4 :: (a -> b -> c -> d -> e      -> z) -> e -> a -> b -> c -> d      -> z
flip5 :: (a -> b -> c -> d -> e -> f -> z) -> f -> a -> b -> c -> d -> e -> z
flip0 = id
flip1 = (flip .) (flip0 .)
flip2 = (flip .) (flip1 .)
flip3 = (flip .) (flip2 .)
flip4 = (flip .) (flip3 .)
flip5 = (flip .) (flip4 .)
