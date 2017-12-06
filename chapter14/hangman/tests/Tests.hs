
module Tests where

import Test.Hspec
import Main

main :: IO ()
main = hspec $ do

  describe "fillInCharacter" $ do
    it "ignores bad guess" $ do
      let word = "puzzle"
      let puzzle = newPuzzle word
      let puzzle' = Puzzle word (map (const Nothing) word) ['x']
      fillInCharacter puzzle 'x' `shouldBe` puzzle'

    it "takes good guess" $ do
      let word = "puzzle"
      let puzzle = newPuzzle word
      let discovered = (Just 'p') : tail (map (const Nothing) word)
      let puzzle' = Puzzle word discovered ['p']
      fillInCharacter puzzle 'p' `shouldBe` puzzle'

  describe "handleGuess" $ do
    it "ignores bad guess" $ do
      let word = "puzzle"
      let puzzle = newPuzzle word
      let puzzle' = Puzzle word (map (const Nothing) word) ['x']
      result <- handleGuess puzzle 'x'
      result `shouldBe` puzzle'

    it "takes good guess" $ do
      let word = "puzzle"
      let puzzle = newPuzzle word
      let discovered = (Just 'p') : tail (map (const Nothing) word)
      let puzzle' = Puzzle word discovered ['p']
      result <- handleGuess puzzle 'p'
      result `shouldBe` puzzle'
