
import Data.List (groupBy)

name = " sergio   anibal  de carvalho   junior  "

splitOn :: Char -> String -> [String]
splitOn d = groupBy split
  where split x y = not (d `elem` [x, y])
