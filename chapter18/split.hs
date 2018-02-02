
import Data.List (groupBy)

name = " sergio   anibal  de carvalho   junior  "

splitOn :: Char -> String -> [String]
splitOn d = foldr acc ("", [])
  where acc c (s, xs)
          | c == d && null s  = ()
