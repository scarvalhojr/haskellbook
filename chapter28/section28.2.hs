
import Criterion.Main (defaultMain, bench, whnf, nf)

infixl 9 !?
_      !? n | n < 0 = Nothing
[]     !? _         = Nothing
(x:_)  !? 0         = Just x
(_:xs) !? n         = xs !? (n-1)

infixl 9 !$
{-# INLINABLE (!$) #-}
xs !$ n
  | n < 0 = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k - 1))
                      (const Nothing) xs n

infixl 9 !%
{-# INLINABLE (!%) #-}
(!%) :: [a] -> Int -> Maybe a
xs !% n
  | n < 0 = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k - 1))
                      (const Nothing) xs n

myList :: [Int]
myList = [0..99999]

main :: IO ()
main = defaultMain
  [ bench "myList !! 99999"
  $ whnf (myList !!) 99999
  , bench "myList !? 99999"
  $ whnf (myList !?) 99999
  , bench "myList !$ 99999"
  $ whnf (myList !$) 99999
  , bench "myList !% 99999"
  $ whnf (myList !%) 99999
  , bench "map list"
  $ nf (map (+1)) myList
  ]
