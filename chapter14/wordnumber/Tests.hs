
module Tests where

import WordNumber (digitToWord, digits, wordNumber)
import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]
    it "returns [9, 8, 7, 6, 5, 4, 3, 2, 1, 0] for 9876543210" $ do
      digits 9876543210 `shouldBe` [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]

  describe "wordNumber" $ do
    it "returns one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "returns nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
    it "returns nine-eight-seven-six-five for 98765" $ do
      wordNumber 98765 `shouldBe` "nine-eight-seven-six-five"
    it "returns four-three-two-one-zero for 43210" $ do
      wordNumber 43210 `shouldBe` "four-three-two-one-zero"
