
module Cipher where

import Data.Char (chr, ord, isAsciiUpper, isAsciiLower)

shiftChar :: Int -> Char -> Char -> Int -> Char
shiftChar shift x base len = chr(start + delta)
  where start = ord base
        delta = (ord x - start + shift) `mod` len

encrypt :: Int -> String -> String
encrypt _ [] = []
encrypt shift (x:xs)
  | isAsciiLower x  = (shiftChar shift x 'a' alphaLen) : encrypt shift xs
  | isAsciiUpper x  = (shiftChar shift x 'A' alphaLen) : encrypt shift xs
  | otherwise       = x : encrypt shift xs
  where alphaLen = 1 + (ord 'z') - (ord 'a')

decrypt :: Int -> String -> String
decrypt shift = encrypt (-shift)
