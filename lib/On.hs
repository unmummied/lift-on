module On where

import Util

class On t where
    onpure :: a -> t a
    (<#>)  :: (t a -> a -> b) -> t (t a -> b)

infixl 4 <#>

instance On ((->) r) where
    onpure       = const
    (<#>)  p x f = p f (f x)

on0' ::                           c                           -> (a -> b) -> c
on1' :: (b                     -> c) -> a                     -> (a -> b) -> c
on2' :: (b -> b                -> c) -> a -> a                -> (a -> b) -> c
on3' :: (b -> b -> b           -> c) -> a -> a -> a           -> (a -> b) -> c
on4' :: (b -> b -> b -> b      -> c) -> a -> a -> a -> a      -> (a -> b) -> c
on5' :: (b -> b -> b -> b -> b -> c) -> a -> a -> a -> a -> a -> (a -> b) -> c
on0' n           = onpure n
on1' u x         = onpure u <#> x
on2' b x y       = onpure b <#> x <#> y
on3' t x y z     = onpure t <#> x <#> y <#> z
on4' q x y z w   = onpure q <#> x <#> y <#> z <#> w
on5' q x y z w v = onpure q <#> x <#> y <#> z <#> w <#> v

on0 ::                           c  -> (a -> b)                          -> c
on1 :: (b                     -> c) -> (a -> b) -> a                     -> c
on2 :: (b -> b                -> c) -> (a -> b) -> a -> a                -> c
on3 :: (b -> b -> b           -> c) -> (a -> b) -> a -> a -> a           -> c
on4 :: (b -> b -> b -> b      -> c) -> (a -> b) -> a -> a -> a -> a      -> c
on5 :: (b -> b -> b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> a -> a -> c
on0 = flip0 . on0'
on1 = flip1 . on1'
on2 = flip2 . on2'
on3 = flip3 . on3'
on4 = flip4 . on4'
on5 = flip5 . on5'

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on = on2
