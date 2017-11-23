
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0     = Nothing
  | x == 0    = Just Zero
  | otherwise = case integerToNat (x - 1) of Nothing -> Nothing
                                             Just n  -> Just (Succ n)
