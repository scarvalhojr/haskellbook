
module Lib
  (Fool(..), half, listOrdered, twice, fourTimes, capitalizeWord)
  where

import Data.Char (toUpper)

data Fool = Fulse | Frue deriving (Eq, Show)

half :: Fractional a => a -> a
half x = x / 2

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, _)       = (Just y, x >= y)

twice :: (a -> a) -> (a -> a)
twice f = f . f

fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord []     = []
capitalizeWord (x:xt) = toUpper x : xt
