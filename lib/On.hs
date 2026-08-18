module On where

onpure :: a -> r -> a
onpure = const

(<#>) :: ((r -> a) -> a -> b) -> r -> (r -> a) -> b
(<#>) p f x = p x (x f)

on0 ::                      c                      -> (a -> b) -> c
on1 :: (b                -> c) -> a                -> (a -> b) -> c
on2 :: (b -> b           -> c) -> a -> a           -> (a -> b) -> c
on3 :: (b -> b -> b      -> c) -> a -> a -> a      -> (a -> b) -> c
on4 :: (b -> b -> b -> b -> c) -> a -> a -> a -> a -> (a -> b) -> c

on0 n         = onpure n
on1 u x       = onpure u <#> x
on2 b x y     = onpure b <#> x <#> y
on3 t x y z   = onpure t <#> x <#> y <#> z
on4 q x y z w = onpure q <#> x <#> y <#> z <#> w
