module Main where

import Test.QuickCheck

import On

prop_on0 :: Int                      -> Fun Int Int                                 -> Bool
prop_on1 :: Int                      -> Fun Int Int -> Fun  Int                 Int -> Bool
prop_on2 :: Int -> Int               -> Fun Int Int -> Fun (Int, Int)           Int -> Bool
prop_on3 :: Int -> Int -> Int        -> Fun Int Int -> Fun (Int, Int, Int)      Int -> Bool
prop_on4 :: Int -> Int -> Int -> Int -> Fun Int Int -> Fun (Int, Int, Int, Int) Int -> Bool

prop_on0 n       (Fun _ f)           = on0 n         f == n
prop_on1 x       (Fun _ f) (Fun _ g) = on1 g x       f == g (f x)
prop_on2 x y     (Fun _ f) (Fun _ g) = on2 t x y     f == t (f x) (f y)             where t a b     = g (a, b)
prop_on3 x y z   (Fun _ f) (Fun _ g) = on3 t x y z   f == t (f x) (f y) (f z)       where t a b c   = g (a, b, c)
prop_on4 x y z w (Fun _ f) (Fun _ g) = on4 t x y z w f == t (f x) (f y) (f z) (f w) where t a b c d = g (a, b, c, d)

main :: IO ()
main = do
    quickCheckWith stdArgs {maxSuccess = 1000} prop_on0
    quickCheckWith stdArgs {maxSuccess = 1000} prop_on1
    quickCheckWith stdArgs {maxSuccess = 1000} prop_on2
    quickCheckWith stdArgs {maxSuccess = 1000} prop_on3
    quickCheckWith stdArgs {maxSuccess = 1000} prop_on4
