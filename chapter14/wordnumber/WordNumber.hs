
module WordNumber
  (digitToWord, digits, wordNumber)
  where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "valid digits are 0 to 9"

digits :: Int -> [Int]
digits = reverse . revdigits
  where revdigits n
          | n < 0     = revdigits (-n)
          | q > 0     = r : revdigits q
          | otherwise = [r]
          where (q, r) = divMod n 10

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
