module Main (main) where

import Test.QuickCheck

import On

prop_on0 :: Int                      -> Fun Int Int                                 -> Bool
prop_on1 :: Int                      -> Fun Int Int -> Fun  Int                 Int -> Bool
prop_on2 :: Int -> Int               -> Fun Int Int -> Fun (Int, Int)           Int -> Bool
prop_on3 :: Int -> Int -> Int        -> Fun Int Int -> Fun (Int, Int, Int)      Int -> Bool
prop_on4 :: Int -> Int -> Int -> Int -> Fun Int Int -> Fun (Int, Int, Int, Int) Int -> Bool

prop_on0 v       (Fun _ f)           = on0 v          f == v
prop_on1 x       (Fun _ f) (Fun _ u ) = on1 u x       f == u (f x)
prop_on2 x y     (Fun _ f) (Fun _ b') = on2 b x y     f == b (f x) (f y)             where b a b     = b' (a, b)
prop_on3 x y z   (Fun _ f) (Fun _ t') = on3 t x y z   f == t (f x) (f y) (f z)       where t a b c   = t' (a, b, c)
prop_on4 x y z w (Fun _ f) (Fun _ q') = on4 q x y z w f == q (f x) (f y) (f z) (f w) where q a b c d = q' (a, b, c, d)

main :: IO ()
main = do
    quickCheckWith stdArgs {maxSuccess = 1000} prop_on0
    quickCheckWith stdArgs {maxSuccess = 1000} prop_on1
    quickCheckWith stdArgs {maxSuccess = 1000} prop_on2
    quickCheckWith stdArgs {maxSuccess = 1000} prop_on3
    quickCheckWith stdArgs {maxSuccess = 1000} prop_on4
