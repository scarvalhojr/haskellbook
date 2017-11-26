module Main where

import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Char (toLower, isAsciiLower)

main :: IO ()
main = forever $ do
  putStrLn $ "Enter input: "
  input <- getLine
  case isPalindrome input of
    True  -> putStrLn "That's a palindrome!"
    False -> do putStrLn "Nope, that's not a palindrome."
                exitSuccess

isPalindrome :: String -> Bool
isPalindrome x = x' == reverse x'
  where x' = filter isAsciiLower (map toLower x)
