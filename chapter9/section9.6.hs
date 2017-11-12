
myWords :: [Char] -> [[Char]]
myWords []       = []
myWords (' ':xs) = myWords xs
myWords xs       = takeWhile (/=' ') xs : (myWords (dropWhile (/=' ') xs))

firstSen  = "Tyger, Tyger, burning bright\n"
secondSen = "In the forest of the night\n"
thirdSen  = "What immortal hand of eye\n"
fourthSen = "Could frame thy fearful\
           \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines []        = []
myLines ('\n':xs) = myLines xs
myLines xs        = takeWhile (/='\n') xs : (myLines (dropWhile (/='\n') xs))

splitByChar :: Char -> String -> [String]
splitByChar _ []        = []
splitByChar c (x:xs)
  | x == c    = splitByChar c xs
  | otherwise = takeWhile (/=c) (x:xs) : (splitByChar c (dropWhile (/=c) (x:xs)))
