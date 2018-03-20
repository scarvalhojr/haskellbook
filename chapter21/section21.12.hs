
import Control.Applicative (liftA, liftA2, liftA3)

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

-- The following isn't necessary
instance Applicative Identity where
  pure                      = Identity
  Identity f <*> Identity x = Identity (f x)

instance Foldable Identity where
  foldr f z (Identity x) = f x z

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

---

newtype Constant a b = Constant { getConstant :: a }
  deriving (Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  -- traverse :: (Applicative f) => (a -> f b) -> Constant a -> f (Constant b)
  traverse f (Constant x) = liftA Constant (pure x)

---

data Optional a = Nada | Yep a
  deriving Show

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep x) = Yep (f x)

instance Foldable Optional where
  -- foldMap :: (Monoid m) => (a -> m) -> Optional a -> m
  foldMap _ Nada    = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  -- traverse :: (Applicative f) => (a -> f b) -> Optional a -> f (Optional b)
  traverse _ Nada    = pure Nada
  traverse f (Yep x) = liftA Yep (f x)

---

data List a = Nil | Cons a (List a)
  deriving Show

instance Functor List where
  fmap _ Nil        = Nil
  fmap f (Cons x t) = Cons (f x) (fmap f t)

instance Foldable List where
  foldMap _ Nil        = mempty
  foldMap f (Cons x t) = (f x) `mappend` (foldMap f t)

instance Traversable List where
  traverse _ Nil        = pure Nil
  traverse f (Cons x t) = liftA2 Cons (f x) (traverse f t)

---

data Three a b c = Three a b c
  deriving Show

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = liftA (Three a b) (f c)

---

data Pair a b = Pair a b
  deriving Show

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Foldable (Pair a) where
  foldMap f (Pair x y) = f y

instance Traversable (Pair a) where
  traverse f (Pair x y) = liftA (Pair x) (f y)

---

data Big a b = Big a b b
  deriving Show

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
  foldMap f (Big x y z) = f y `mappend` f z

instance Traversable (Big a) where
  traverse f (Big x y z) = liftA2 (Big x) (f y) (f z)

---

-- ???
data S n a = S (n a) a
  deriving Show

---

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  -- foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
  foldMap _ Empty        = mempty
  foldMap f (Leaf x)     = f x
  foldMap f (Node l x r) = (foldMap f l) `mappend` (f x) `mappend` (foldMap f r)

instance Traversable Tree where
  -- traverse :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Empty        = pure Empty
  traverse f (Leaf x)     = liftA Leaf (f x)
  traverse f (Node l x r) = liftA3 Node (traverse f l) (f x) (traverse f r)
