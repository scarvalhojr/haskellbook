
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else [x * x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else []

---

data Sum a b = First a | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure x = Second x

  (First a)  <*> _          = First a
  _          <*> (First a)  = First a
  (Second f) <*> (Second x) = Second (f x)

instance Monad (Sum a) where
  return = pure

  (First a)  >>= _ = First a
  (Second b) >>= f = f b
