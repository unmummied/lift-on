module Main (main) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import On

curry2 :: ((a, b      ) -> e) -> a -> b           -> e
curry3 :: ((a, b, c   ) -> e) -> a -> b -> c      -> e
curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry2 b = curry b
curry3 t = ((.) . (.) . (.)) t (,,)
curry4 q = ((.) . (.) . (.) . (.)) q (,,,)

prop_on0 ::                          Int                             -> Fun Int Int -> Expectation
prop_on1 :: Fun  Int                 Int -> Int                      -> Fun Int Int -> Expectation
prop_on2 :: Fun (Int, Int)           Int -> Int -> Int               -> Fun Int Int -> Expectation
prop_on3 :: Fun (Int, Int, Int)      Int -> Int -> Int -> Int        -> Fun Int Int -> Expectation
prop_on4 :: Fun (Int, Int, Int, Int) Int -> Int -> Int -> Int -> Int -> Fun Int Int -> Expectation
prop_on0        v           (Fun _ f) = on0 v         f `shouldBe` v
prop_on1 (Fun _ u ) x       (Fun _ f) = on1 u x       f `shouldBe` u (f x)
prop_on2 (Fun _ b') x y     (Fun _ f) = on2 b x y     f `shouldBe` b (f x) (f y)             where b = curry2 b'
prop_on3 (Fun _ t') x y z   (Fun _ f) = on3 t x y z   f `shouldBe` t (f x) (f y) (f z)       where t = curry3 t'
prop_on4 (Fun _ q') x y z w (Fun _ f) = on4 q x y z w f `shouldBe` q (f x) (f y) (f z) (f w) where q = curry4 q'

prop_on0Id ::                          Int                             -> Expectation
prop_on1Id :: Fun  Int                 Int -> Int                      -> Expectation
prop_on2Id :: Fun (Int, Int)           Int -> Int -> Int               -> Expectation
prop_on3Id :: Fun (Int, Int, Int)      Int -> Int -> Int -> Int        -> Expectation
prop_on4Id :: Fun (Int, Int, Int, Int) Int -> Int -> Int -> Int -> Int -> Expectation
prop_on0Id        v           = on0 v         id `shouldBe` v
prop_on1Id (Fun _ u ) x       = on1 u x       id `shouldBe` u x
prop_on2Id (Fun _ b') x y     = on2 b x y     id `shouldBe` b x y     where b = curry2 b'
prop_on3Id (Fun _ t') x y z   = on3 t x y z   id `shouldBe` t x y z   where t = curry3 t'
prop_on4Id (Fun _ q') x y z w = on4 q x y z w id `shouldBe` q x y z w where q = curry4 q'

prop_on0Comp ::                          Int                             -> Fun Int Int -> Fun Int Int -> Expectation
prop_on1Comp :: Fun  Int                 Int -> Int                      -> Fun Int Int -> Fun Int Int -> Expectation
prop_on2Comp :: Fun (Int, Int)           Int -> Int -> Int               -> Fun Int Int -> Fun Int Int -> Expectation
prop_on3Comp :: Fun (Int, Int, Int)      Int -> Int -> Int -> Int        -> Fun Int Int -> Fun Int Int -> Expectation
prop_on4Comp :: Fun (Int, Int, Int, Int) Int -> Int -> Int -> Int -> Int -> Fun Int Int -> Fun Int Int -> Expectation
prop_on0Comp        v           (Fun _ f) (Fun _ g) = on0' (on0' v f) g         `shouldBe` on0' v (f . g)
prop_on1Comp (Fun _ u ) x       (Fun _ f) (Fun _ g) = on1' (on1' u f) g x       `shouldBe` on1' u (f . g) x
prop_on2Comp (Fun _ b') x y     (Fun _ f) (Fun _ g) = on2' (on2' b f) g x y     `shouldBe` on2' b (f . g) x y     where b = curry2 b'
prop_on3Comp (Fun _ t') x y z   (Fun _ f) (Fun _ g) = on3' (on3' t f) g x y z   `shouldBe` on3' t (f . g) x y z   where t = curry3 t'
prop_on4Comp (Fun _ q') x y z w (Fun _ f) (Fun _ g) = on4' (on4' q f) g x y z w `shouldBe` on4' q (f . g) x y z w where q = curry4 q'

prop_on0Flip ::                          Int                             -> Fun Int Int -> Fun Int Int -> Expectation
prop_on1Flip :: Fun  Int                 Int -> Int                      -> Fun Int Int -> Fun Int Int -> Expectation
prop_on2Flip :: Fun (Int, Int)           Int -> Int -> Int               -> Fun Int Int -> Fun Int Int -> Expectation
prop_on3Flip :: Fun (Int, Int, Int)      Int -> Int -> Int -> Int        -> Fun Int Int -> Fun Int Int -> Expectation
prop_on4Flip :: Fun (Int, Int, Int, Int) Int -> Int -> Int -> Int -> Int -> Fun Int Int -> Fun Int Int -> Expectation
prop_on0Flip        v           (Fun _ f) (Fun _ g) = (flip on0' f . flip on0' g) v         `shouldBe` flip on0' (g . f) v
prop_on1Flip (Fun _ u ) x       (Fun _ f) (Fun _ g) = (flip on1' f . flip on1' g) u x       `shouldBe` flip on1' (g . f) u x
prop_on2Flip (Fun _ b') x y     (Fun _ f) (Fun _ g) = (flip on2' f . flip on2' g) b x y     `shouldBe` flip on2' (g . f) b x y     where b = curry2 b'
prop_on3Flip (Fun _ t') x y z   (Fun _ f) (Fun _ g) = (flip on3' f . flip on3' g) t x y z   `shouldBe` flip on3' (g . f) t x y z   where t = curry3 t'
prop_on4Flip (Fun _ q') x y z w (Fun _ f) (Fun _ g) = (flip on4' f . flip on4' g) q x y z w `shouldBe` flip on4' (g . f) q x y z w where q = curry4 q'

main :: IO ()
main = hspec $ modifyMaxSuccess (const 1000) $ do
    describe "on" $ do
        it "on0" $ property prop_on0
        it "on1" $ property prop_on1
        it "on2" $ property prop_on2
        it "on3" $ property prop_on3
        it "on4" $ property prop_on4

    describe "identity" $ do
        it "on0" $ property prop_on0Id
        it "on1" $ property prop_on1Id
        it "on2" $ property prop_on2Id
        it "on3" $ property prop_on3Id
        it "on4" $ property prop_on4Id

    describe "composition" $ do
        it "on0" $ property prop_on0Comp
        it "on1" $ property prop_on1Comp
        it "on2" $ property prop_on2Comp
        it "on3" $ property prop_on3Comp
        it "on4" $ property prop_on4Comp

    describe "flip" $ do
        it "on0" $ property prop_on0Flip
        it "on1" $ property prop_on1Flip
        it "on2" $ property prop_on2Flip
        it "on3" $ property prop_on3Flip
        it "on4" $ property prop_on4Flip
