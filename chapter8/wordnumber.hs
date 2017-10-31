
module WordNumber where

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

digits :: Int -> [Int]
digits = reverse . revdigits
  where revdigits n
          | n < 0     = revdigits (-n)
          | quot > 0  = rem : revdigits quot
          | otherwise = [rem]
          where quot = n `div` 10
                rem  = n `mod` 10

wordNumber :: Int -> String
wordNumber = concat . intersperse " - " . map digitToWord . digits
