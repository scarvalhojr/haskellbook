
import Data.Monoid ((<>))

data Constant a b = Constant a
  deriving Show

instance Foldable (Constant a) where
  foldMap _ _ = mempty

---

data Two a b = Two a b
  deriving Show

instance Foldable (Two a) where
  foldr f z (Two _ b) = f b z

---

data Three a b c = Three a b c
  deriving Show

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

---

data Three' a b = Three' a b b
  deriving Show

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = (f b) <> (f b')

---

data Four a b = Four a b b b
  deriving Show

instance Foldable (Four a) where
  foldMap f (Four _ b b' b'') = f b <> f b' <> f b''

---

filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
           => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)

---

greaterThanFive = (>5)
oneToTen = [1..10]
test = filter greaterThanFive oneToTen == filterF greaterThanFive oneToTen
