
module Tests where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.Modifiers (NonZero, NonNegative)
import Data.List (sort)
import Lib (Fool(..), half, listOrdered, twice, fourTimes, capitalizeWord)

prop_HalfIdentity :: Float -> Bool
prop_HalfIdentity x = 2 * half x == x

prop_IntListSorted :: [Int] -> Bool
prop_IntListSorted xs = listOrdered (sort xs) == True

prop_CharListSorted :: [Char] -> Bool
prop_CharListSorted xs = listOrdered (sort xs) == True

prop_IntListAnyOrder :: [Int] -> Bool
prop_IntListAnyOrder xs
  | xs == sort xs  = listOrdered xs == True
  | otherwise      = listOrdered xs == False

prop_PlusAssociativeInt :: Int -> Int -> Int -> Bool
prop_PlusAssociativeInt = prop_PlusAssociative

-- Does not hold due to rounding errors!
prop_PlusAssociativeFloat :: Float -> Float -> Float -> Bool
prop_PlusAssociativeFloat = prop_PlusAssociative

prop_PlusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
prop_PlusAssociative x y z = x + (y + z) == (x + y) + z

prop_PlusCommutative :: Int -> Int -> Bool
prop_PlusCommutative x y = x + y == y + x

prop_MultAssociative :: Int -> Int -> Int -> Bool
prop_MultAssociative x y z = x * (y * z) == (x * y) * z

prop_MultCommutative :: Int -> Int -> Bool
prop_MultCommutative x y = x * y == y * x

prop_QuotAndRem :: Int -> (NonZero Int) -> Bool
prop_QuotAndRem x (NonZero y) = y * (quot x y) + (rem x y) == x

prop_DivAndMod :: Int -> (NonZero Int) -> Bool
prop_DivAndMod x (NonZero y) = y * (div x y) + (mod x y) == x

-- Does not hold:
prop_ExpAssociative :: Int -> Int -> Int -> Bool
prop_ExpAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

-- Does not hold:
prop_ExpCommutative :: Int -> Int -> Bool
prop_ExpCommutative x y = x ^ y == y ^ x

prop_DoubleReverseIsIdentity :: [Int] -> Bool
prop_DoubleReverseIsIdentity xs = reverse (reverse xs) == xs

prop_Dollar :: (Eq b, Show b) => (a -> b) -> a -> Bool
prop_Dollar f x = f x == (f $ x)

prop_DollarAddOne :: Int -> Bool
prop_DollarAddOne = prop_Dollar (+1)

prop_FuncComposition :: (Eq c) => (b -> c) -> (a -> b) -> a -> Bool
prop_FuncComposition f g x = (f . g) x == f (g x)

prop_FuncCompositionRepeatTakeHead :: Int -> Bool
prop_FuncCompositionRepeatTakeHead = prop_FuncComposition (head) (\x -> take 10 (repeat x))

-- Does not hold:
prop_FoldrConsIsConcat :: [Int] -> [Int] -> Bool
prop_FoldrConsIsConcat xs ys = foldr (:) xs ys == (++) xs ys

prop_FoldrConcatIsConcat :: [[Int]] -> Bool
prop_FoldrConcatIsConcat xs = foldr (++) [] xs == concat xs

-- Does not hold:
prop_TakeNFromList :: (NonNegative Int) -> [Int] -> Bool
prop_TakeNFromList (NonNegative n) xs = length (take n xs) == n

prop_ReadAndShow :: Int -> Bool
prop_ReadAndShow x = (read (show x)) == x

-- Holds if numbers are non-negative
prop_SquareIdentity :: (NonNegative Float) -> Bool
prop_SquareIdentity (NonNegative x) = sqrt (x * x) == x

-- Funnily enough, this one doesn't hold:
prop_SquareIdentity' :: (NonNegative Float) -> Bool
prop_SquareIdentity' (NonNegative x) = (sqrt x) * (sqrt x) == x

prop_CapitalizeWordIdempotence :: String -> Bool
prop_CapitalizeWordIdempotence s =
  (capitalizeWord s == twice capitalizeWord s) &&
  (capitalizeWord s == fourTimes capitalizeWord s)

prop_SortIdempotence :: [Int] -> Bool
prop_SortIdempotence xs =
  (sort xs == twice sort xs) && (sort xs == fourTimes sort xs)

foolGen :: Gen Fool
foolGen = oneof [return Fulse, return Frue]

foolGenMoreFulse :: Gen Fool
foolGenMoreFulse = frequency [(2, return Fulse), (1, return Frue)]

main :: IO ()
main = do quickCheck prop_HalfIdentity
          quickCheck prop_IntListSorted
          quickCheck prop_CharListSorted
          quickCheck prop_IntListAnyOrder
          quickCheck prop_PlusAssociativeInt
          -- quickCheck prop_PlusAssociativeFloat
          quickCheck prop_PlusCommutative
          quickCheck prop_MultAssociative
          quickCheck prop_MultCommutative
          quickCheck prop_QuotAndRem
          quickCheck prop_DivAndMod
          -- Does not hold:
          -- quickCheck prop_ExpAssociative
          -- quickCheck prop_ExpCommutative
          quickCheck prop_DoubleReverseIsIdentity
          quickCheck prop_DollarAddOne
          quickCheck prop_FuncCompositionRepeatTakeHead
          -- Does not hold:
          -- quickCheck prop_FoldrConsIsConcat
          quickCheck prop_FoldrConcatIsConcat
          -- Does not hold:
          -- quickCheck prop_TakeNFromList
          quickCheck prop_ReadAndShow
          quickCheck prop_SquareIdentity
          -- Does not hold:
          -- quickCheck prop_SquareIdentity'
          quickCheck prop_CapitalizeWordIdempotence
          quickCheck prop_SortIdempotence
