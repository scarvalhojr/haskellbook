
module Multiplication
  (mult)
  where

import Test.Hspec
import Test.QuickCheck

mult :: (Ord a, Integral a) => a -> a -> a
mult m n
  | m == 0    = 0
  | m < 0     = mult (-m) (-n)
  | otherwise = n + mult (m - 1) n

main :: IO ()
main = hspec $ do
  describe "Multiplication" $ do
    it "mult 1 0 is equal to 0" $ do
      mult 1 0 `shouldBe` 0
    it "mult 0 1 is equal to 0" $ do
      mult 0 1 `shouldBe` 0
    it "mult 1 2 is equal to 2" $ do
      mult 1 2 `shouldBe` 2
    it "mult 2 1 is equal to 2" $ do
      mult 2 1 `shouldBe` 2
    it "mult (-2) 1 is equal to -2" $ do
      mult (-2) 1 `shouldBe` (-2)
    it "mult 2 (-1) is equal to -2" $ do
      mult (2) (-1) `shouldBe` (-2)
    it "mult (-2) (-1) is equal to 2" $ do
      mult (-2) (-1) `shouldBe` 2
    it "mult 9 9 is equal to 81" $ do
      mult 9 9 `shouldBe` 81
    it "mult x 0 is equal to 0" $ do
      property $ \x -> mult x 1 == (0 :: Int)

propMultIdentity :: Int -> Bool
propMultIdentity x = mult x 1 == x

runQC :: IO ()
runQC = quickCheck propMultIdentity
