module Main (main) where

import Test.QuickCheck

import On

curry2 :: ((a, b      ) -> e) -> a -> b           -> e
curry3 :: ((a, b, c   ) -> e) -> a -> b -> c      -> e
curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry2 b = curry b
curry3 t = ((.) . (.) . (.)) t (,,)
curry4 q = ((.) . (.) . (.) . (.)) q (,,,)

prop_on0 ::                          Int                             -> Fun Int Int -> Bool
prop_on1 :: Fun  Int                 Int -> Int                      -> Fun Int Int -> Bool
prop_on2 :: Fun (Int, Int)           Int -> Int -> Int               -> Fun Int Int -> Bool
prop_on3 :: Fun (Int, Int, Int)      Int -> Int -> Int -> Int        -> Fun Int Int -> Bool
prop_on4 :: Fun (Int, Int, Int, Int) Int -> Int -> Int -> Int -> Int -> Fun Int Int -> Bool
prop_on0        v           (Fun _ f) = on0 v         f == v
prop_on1 (Fun _ u ) x       (Fun _ f) = on1 u x       f == u (f x)
prop_on2 (Fun _ b') x y     (Fun _ f) = on2 b x y     f == b (f x) (f y)             where b = curry2 b'
prop_on3 (Fun _ t') x y z   (Fun _ f) = on3 t x y z   f == t (f x) (f y) (f z)       where t = curry3 t'
prop_on4 (Fun _ q') x y z w (Fun _ f) = on4 q x y z w f == q (f x) (f y) (f z) (f w) where q = curry4 q'

-- identity
prop_on0Id ::                          Int                             -> Bool
prop_on1Id :: Fun  Int                 Int -> Int                      -> Bool
prop_on2Id :: Fun (Int, Int)           Int -> Int -> Int               -> Bool
prop_on3Id :: Fun (Int, Int, Int)      Int -> Int -> Int -> Int        -> Bool
prop_on4Id :: Fun (Int, Int, Int, Int) Int -> Int -> Int -> Int -> Int -> Bool
prop_on0Id        v           = on0 v         id == v
prop_on1Id (Fun _ u ) x       = on1 u x       id == u x
prop_on2Id (Fun _ b') x y     = on2 b x y     id == b x y     where b = curry2 b'
prop_on3Id (Fun _ t') x y z   = on3 t x y z   id == t x y z   where t = curry3 t'
prop_on4Id (Fun _ q') x y z w = on4 q x y z w id == q x y z w where q = curry4 q'

-- composition
prop_on0Comp ::                          Int                             -> Fun Int Int -> Fun Int Int -> Bool
prop_on1Comp :: Fun  Int                 Int -> Int                      -> Fun Int Int -> Fun Int Int -> Bool
prop_on2Comp :: Fun (Int, Int)           Int -> Int -> Int               -> Fun Int Int -> Fun Int Int -> Bool
prop_on3Comp :: Fun (Int, Int, Int)      Int -> Int -> Int -> Int        -> Fun Int Int -> Fun Int Int -> Bool
prop_on4Comp :: Fun (Int, Int, Int, Int) Int -> Int -> Int -> Int -> Int -> Fun Int Int -> Fun Int Int -> Bool
prop_on0Comp        v           (Fun _ f) (Fun _ g) = on0' (on0' v f) g         == on0' v (f . g)
prop_on1Comp (Fun _ u ) x       (Fun _ f) (Fun _ g) = on1' (on1' u f) g x       == on1' u (f . g) x
prop_on2Comp (Fun _ b') x y     (Fun _ f) (Fun _ g) = on2' (on2' b f) g x y     == on2' b (f . g) x y     where b = curry2 b'
prop_on3Comp (Fun _ t') x y z   (Fun _ f) (Fun _ g) = on3' (on3' t f) g x y z   == on3' t (f . g) x y z   where t = curry3 t'
prop_on4Comp (Fun _ q') x y z w (Fun _ f) (Fun _ g) = on4' (on4' q f) g x y z w == on4' q (f . g) x y z w where q = curry4 q'

-- flip
prop_on0Flip ::                          Int                             -> Fun Int Int -> Fun Int Int -> Bool
prop_on1Flip :: Fun  Int                 Int -> Int                      -> Fun Int Int -> Fun Int Int -> Bool
prop_on2Flip :: Fun (Int, Int)           Int -> Int -> Int               -> Fun Int Int -> Fun Int Int -> Bool
prop_on3Flip :: Fun (Int, Int, Int)      Int -> Int -> Int -> Int        -> Fun Int Int -> Fun Int Int -> Bool
prop_on4Flip :: Fun (Int, Int, Int, Int) Int -> Int -> Int -> Int -> Int -> Fun Int Int -> Fun Int Int -> Bool
prop_on0Flip        v           (Fun _ f) (Fun _ g) = (flip on0' f . flip on0' g) v         == flip on0' (g . f) v
prop_on1Flip (Fun _ u ) x       (Fun _ f) (Fun _ g) = (flip on1' f . flip on1' g) u x       == flip on1' (g . f) u x
prop_on2Flip (Fun _ b') x y     (Fun _ f) (Fun _ g) = (flip on2' f . flip on2' g) b x y     == flip on2' (g . f) b x y     where b = curry2 b'
prop_on3Flip (Fun _ t') x y z   (Fun _ f) (Fun _ g) = (flip on3' f . flip on3' g) t x y z   == flip on3' (g . f) t x y z   where t = curry3 t'
prop_on4Flip (Fun _ q') x y z w (Fun _ f) (Fun _ g) = (flip on4' f . flip on4' g) q x y z w == flip on4' (g . f) q x y z w where q = curry4 q'

main :: IO ()
main = do
    let args = stdArgs {maxSuccess = 1000}

    quickCheckWith args prop_on0
    quickCheckWith args prop_on1
    quickCheckWith args prop_on2
    quickCheckWith args prop_on3
    quickCheckWith args prop_on4

    quickCheckWith args prop_on0Id
    quickCheckWith args prop_on1Id
    quickCheckWith args prop_on2Id
    quickCheckWith args prop_on3Id
    quickCheckWith args prop_on4Id

    quickCheckWith args prop_on0Comp
    quickCheckWith args prop_on1Comp
    quickCheckWith args prop_on2Comp
    quickCheckWith args prop_on3Comp
    quickCheckWith args prop_on4Comp

    quickCheckWith args prop_on0Flip
    quickCheckWith args prop_on1Flip
    quickCheckWith args prop_on2Flip
    quickCheckWith args prop_on3Flip
    quickCheckWith args prop_on4Flip
