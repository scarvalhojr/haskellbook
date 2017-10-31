
f :: Ord a => a -> a -> Bool
f = undefined

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where xLast = fst (x `divMod` 10)
        d     = snd (xLast `divMod` 10)

hundredsDigit :: Integral a => a -> a
hundredsDigit x = d
  where xLast = x `div` 100
        d     = xLast `mod` 10

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True  = y

foldBool :: a -> a -> Bool -> a
foldBool x y c
  | c         = y
  | otherwise = x

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
