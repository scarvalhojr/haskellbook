
import Data.Char

filterUpper :: String -> String
filterUpper []  = []
filterUpper (x:xs)
  | isUpper x   = x : filterUpper xs
  | otherwise   = filterUpper xs

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs

capitalizeAll :: String -> String
capitalizeAll []     = []
capitalizeAll (x:xs) = toUpper x : capitalizeAll xs

capitalizeFirstLetter :: String -> Char
capitalizeFirstLetter xs = toUpper (head xs)

capitalizeFirstLetter' :: String -> Char
capitalizeFirstLetter' xs = toUpper (head xs)


myAnd :: [Bool] -> Bool
myAnd []         = True
myAnd (False:xs) = False
myAnd (True:xs)  = myAnd xs

myAnd' :: [Bool] -> Bool
myAnd' []     = True
myAnd' (x:xs)
  | x         = myAnd xs
  | otherwise = False

myOr :: [Bool] -> Bool
myOr []         = False
myOr (True:xs)  = True
myOr (False:xs) = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs)
  | f x        = True
  | otherwise  = myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ []   = False
myElem x (y:ys)
  | x == y    = True
  | otherwise = myElem x ys

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = (myReverse xs) ++ [x]

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = (f x) ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ []     = error "empty list"
myMaximumBy f (x:xs) = go f x xs
  where go _ max []  = max
        go f max (x:xs)
          | f max x == LT  = go f x xs
          | otherwise      = go f max xs

myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ []     = error "empty list"
myMinimumBy f (x:xs) = go f x xs
  where go _ min []  = min
        go f min (x:xs)
          | f min x == GT  = go f x xs
          | otherwise      = go f min xs

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare
