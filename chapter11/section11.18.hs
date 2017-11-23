
import Data.Char (toUpper)

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _  = True
isSubseqOf _  [] = False
isSubseqOf xs@(x:xt) (y:yt)
  | x == y     = isSubseqOf xt yt
  | otherwise  = isSubseqOf xs yt

isSubseqOfWorks = (isSubseqOf "blah" "blahwoot" == True)
               && (isSubseqOf "blah" "wootblah" == True)
               && (isSubseqOf "blah" "wboloath" == True)
               && (isSubseqOf "blah" "wootbla"  == False)
               && (isSubseqOf "blah" "halbwoot" == False)
               && (isSubseqOf "blah" "blawhoot" == True)
               && (isSubseqOf "blah" "bbblahhh" == True)
               && (isSubseqOf "blah" "bbblaahh" == True)

isContiguousSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isContiguousSubseqOf [] _  = True
isContiguousSubseqOf _  [] = False
isContiguousSubseqOf xs@(x:xt) (y:yt)
  | x == y     = isPrefixOf xt yt || isContiguousSubseqOf xs yt
  | otherwise  = isContiguousSubseqOf xs yt
  where isPrefixOf [] _          = True
        isPrefixOf _  []         = False
        isPrefixOf (x:xt) (y:yt) = (x == y) && isPrefixOf xt yt

isContiguousSubseqOfWorks = (isContiguousSubseqOf "blah" "blahwoot" == True)
                         && (isContiguousSubseqOf "blah" "wootblah" == True)
                         && (isContiguousSubseqOf "blah" "wboloath" == False)
                         && (isContiguousSubseqOf "blah" "wootbla"  == False)
                         && (isContiguousSubseqOf "blah" "halbwoot" == False)
                         && (isContiguousSubseqOf "blah" "blawhoot" == False)
                         && (isContiguousSubseqOf "blah" "bbblahhh" == True)
                         && (isContiguousSubseqOf "blah" "bbblaahh" == False)

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map capitalize (words s)
  where capitalize xs@(x:xt) = (xs, toUpper x : xt)

capitalizeWordsWorks = capitalizeWords "hello world" ==
                       [("hello", "Hello"), ("world", "World")]

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (' ':xt) = ' ' : capitalizeWord xt
capitalizeWord (x:xt)   = toUpper x : xt

capitalizeWordWorks = (capitalizeWord "Chortle" == "Chortle")
                   && (capitalizeWord "chortle" == "Chortle")
                   && (capitalizeWord " chortle" == " Chortle")

splitOn :: Char -> String -> [String]
splitOn d = foldr f [[]]
  where f c xs@(x:xt)
          | c == d    = [c]:xs
          | otherwise = (c:x):xt

capitalizeParagraph :: String -> String
capitalizeParagraph t = concat (map capitalizeWord (splitOn '.' t))

capitalizeParagraphWorks = (capitalizeParagraph "blah. woot ha." ==
                            "Blah. Woot ha.")
                        && (capitalizeParagraph "  blah.  woot ha.  " ==
                            "  Blah.  Woot ha.  ")
                        && (capitalizeParagraph "  blah.  woot ha  " ==
                            "  Blah.  Woot ha  ")

main :: IO ()
main = do
        putStrLn $ "isSubseqOfWorks: " ++ show isSubseqOfWorks
        putStrLn $ "isContiguousSubseqOfWorks: " ++ show isContiguousSubseqOfWorks
        putStrLn $ "capitalizeWordsWorks: " ++ show capitalizeWordsWorks
        putStrLn $ "capitalizeWordWorks: " ++ show capitalizeWordWorks
        putStrLn $ "capitalizeParagraphWorks: " ++ show capitalizeParagraphWorks
