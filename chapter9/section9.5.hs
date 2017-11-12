eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True  = [False, True]
eftBool True  False = []
eftBool True  True  = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd f t
  | f > t     = []
  | f == t    = [t]
  | otherwise = f : (eftOrd (succ f) t)

eftInt :: Int -> Int -> [Int]
eftInt f t
  | f > t      = []
  | f == t     = [f]
  | otherwise  = f : (eftInt (succ f) t)

eftChar :: Char -> Char -> [Char]
eftChar f t
  | f > t      = []
  | f == t     = [f]
  | otherwise  = f : (eftChar (succ f) t)
