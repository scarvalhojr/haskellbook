
newtype HumanWord = HumanWord String deriving (Eq, Show)

vowels = "aeiou"

makeHumanWord :: String -> Maybe HumanWord
makeHumanWord str
  | vow > con  = Nothing
  | otherwise  = Just (HumanWord str)
  where vow = length $ filter (`elem` vowels) str
        con = (length str) - vow
