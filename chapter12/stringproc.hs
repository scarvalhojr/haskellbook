
import Data.Char (toLower)

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

replaceThe :: String -> String
replaceThe = unwords . map replace . words
  where replace w = case notThe w of Nothing -> "a"
                                     Just w' -> w'

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = count . words
  where count []      = 0
        count (x:[])  = 0
        count ("the":y:z)
          | toLower (head y) `elem` "aeiou" = 1 + count z
          | otherwise                       = count (y:z)
        count (x:y)   = count y

isVowel :: Char -> Bool
isVowel = (`elem` "aeiou") . toLower

onlyVowels :: String -> [Char]
onlyVowels = filter isVowel

countVowels :: String -> Int
countVowels = length . onlyVowels
