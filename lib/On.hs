module On where

class On t where
    onpure :: a -> t a
    onap :: (t a -> a -> b) -> t (t a -> b)

instance On ((->) r) where
    onpure = const
    onap p x f = p f (f x)

on0 ::                      c                      -> (a -> b) -> c
on1 :: (b                -> c) -> a                -> (a -> b) -> c
on2 :: (b -> b           -> c) -> a -> a           -> (a -> b) -> c
on3 :: (b -> b -> b      -> c) -> a -> a -> a      -> (a -> b) -> c
on4 :: (b -> b -> b -> b -> c) -> a -> a -> a -> a -> (a -> b) -> c

on0 v         = onpure v
on1 u x       = onpure u `onap` x
on2 b x y     = onpure b `onap` x `onap` y
on3 t x y z   = onpure t `onap` x `onap` y `onap` z
on4 q x y z w = onpure q `onap` x `onap` y `onap` z `onap` w

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on b u x y = on2 b x y u
