
recurMap :: [[a]] -> [[[a]]]
recurMap tr
  | null tr = [[[]]]
  | otherwise = map (\x -> map (:x) (head tr)) (concat (recurMap (tail tr)))

stops = "pbtdkg"
vowels = "aeiou"

stopVowelStop = [(x,y,z) | x <- stops, y <- vowels, z <- stops]

stopVowelStop' = [(x,y,z) | x <- stops, y <- vowels, z <- stops, x == 'p']

nouns = ["boy", "girl", "dog"]
verbs = ["kills", "kisses", "chases"]
nounVerbNoun = [(x,y,z) | x <- nouns, y <- verbs, z <- nouns]

-- average length of words in a string
seekritFunc x = div (sum (map length (words x))) (length (words x))

seekritFunc' x = total_chars / num_words
  where total_chars = fromIntegral (sum (map length (words x)))
        num_words   = fromIntegral (length (words x))

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> acc || f x) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\y acc -> acc || x == y) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (==x)

myReverse :: [a] -> [a]
myReverse = foldr (\x ys -> ys ++ [x]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> (f x) : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr go []
  where go x acc
          | f x        = x : acc
          | otherwise  = acc

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x acc -> (f x) ++ acc) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr go x xs
  where go x max
          | f x max == GT   = x
          | otherwise       = max

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr go x xs
  where go x min
          | f x min == LT  = x
          | otherwise      = min
