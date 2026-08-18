module On where

class On t where
    onpure :: a -> t a
    (<#>) :: (t a -> a -> b) -> t (t a -> b)

infixl 4 <#>

instance On ((->) r) where
    -- onpure :: a -> r -> a
    onpure = const

    -- (<#>) :: ((r -> a) -> a -> b) -> r -> (r -> a) -> b
    (<#>) p x f = p f (f x)

on0 ::                      c                      -> (a -> b) -> c
on1 :: (b                -> c) -> a                -> (a -> b) -> c
on2 :: (b -> b           -> c) -> a -> a           -> (a -> b) -> c
on3 :: (b -> b -> b      -> c) -> a -> a -> a      -> (a -> b) -> c
on4 :: (b -> b -> b -> b -> c) -> a -> a -> a -> a -> (a -> b) -> c

on0 v         = onpure v
on1 u x       = onpure u <#> x
on2 b x y     = onpure b <#> x <#> y
on3 t x y z   = onpure t <#> x <#> y <#> z
on4 q x y z w = onpure q <#> x <#> y <#> z <#> w
