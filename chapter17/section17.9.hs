
import Data.Monoid ((<>))
import Control.Applicative (liftA3)


data Pair a = Pair a a
  deriving Show

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a

  (Pair f g) <*> (Pair a a') = Pair (f a) (g a')

---

data Two a b = Two a b
  deriving Show

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b

  (Two a f) <*> (Two a' b) = Two (a <> a') (f b)

---

data Three a b c = Three a b c
  deriving Show

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c

  (Three a b f) <*> (Three a' b' c) = Three (a <> a') (b <> b') (f c)

---

data Threeish a b = Threeish a b b
  deriving Show

instance Functor (Threeish a) where
  fmap f (Threeish a b b') = Threeish a (f b) (f b')

instance Monoid a => Applicative (Threeish a) where
  pure b = Threeish mempty b b

  (Threeish a f g) <*> (Threeish a' b b') = Threeish (a <> a') (f b) (g b')

---

data Four a b c d = Four a b c d
  deriving Show

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d

  (Four a b c f) <*> (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (f d)

---

data Fourish a b = Fourish a a a b
  deriving Show

instance Functor (Fourish a) where
  fmap f (Fourish a a' a'' b) = Fourish a a' a'' (f b)

instance Monoid a => Applicative (Fourish a) where
  pure b = Fourish mempty mempty mempty b

  (Fourish x y z f) <*> (Fourish x' y' z' b) = Fourish (x <> x') (y <> y') (z <> z') (f b)

---

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

triplets :: [(Char,Char,Char)]
triplets = combos stops vowels stops
