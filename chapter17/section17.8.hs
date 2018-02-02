
import Data.Monoid ((<>))


data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil        = Nil
  fmap f (Cons x t) = Cons (f x) (fmap f t)

instance Monoid (List a) where
  mempty = Nil

  mappend Nil         ys = ys
  mappend (Cons x xs) ys = Cons x (xs <> ys)

instance Applicative List where
  pure x = Cons x Nil

  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) fs xs = flatMap (flip fmap xs) fs

toList :: Foldable t => t a -> List a
toList = foldr Cons Nil

fromList :: List a -> [a]
fromList Nil        = []
fromList (Cons h t) = h : fromList t

fold :: (a -> b -> b) -> b -> List a -> b
fold _ z Nil        = z
fold f z (Cons h t) = f h (fold f z t)

concat' :: List (List a) -> List a
concat' = fold (<>) Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f l = concat' (fmap f l)

---

f1 = toList [(+1), (*2)]
v1 = toList [1, 2]

test1 = (f1 <*> v1) == toList [2, 3, 2, 4]

f2 = toList [negate, (+10), (*10), (`mod` 10)]
v2 = toList [50..55]

test2 = fromList (f2 <*> v2) == [-50, -51, -52, -53, -54, -55
                                , 60,  61,  62,  63,  64,  65
                                ,500, 510, 520, 530, 540, 550
                                ,  0,   1,   2,   3,   4,   5]

---

newtype ZipList a = ZipList (List a)
  deriving (Eq, Show)

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (fmap f xs)

instance Monoid (ZipList a) where
  mempty = ZipList Nil
  mappend (ZipList xs) (ZipList ys) = ZipList (xs <> ys)

instance Applicative ZipList where
  pure x = ZipList (Cons x Nil)

  (<*>) (ZipList Nil) _ = ZipList Nil
  (<*>) _ (ZipList Nil) = ZipList Nil
  (<*>) (ZipList (Cons f fs)) (ZipList (Cons x xs))
    = ZipList (Cons (f x) Nil) <> (ZipList fs <*> ZipList xs)

---

f3 = ZipList (toList [(+9), (*2), (+8)])
v3 = ZipList (toList [1..3])

test3 = (f3 <*> v3) == ZipList (toList [10, 4, 11])

f4 = f3
v4 = ZipList (toList (repeat 1))

test4 = (f4 <*> v4) == ZipList (toList [10, 2, 9])

f5 = ZipList (toList [negate, (+10), (*10), (`mod` 10)])
v5 = ZipList (toList [50..])

test5 = (f5 <*> v5) == ZipList (toList [-50, 61, 520, 3])

---

data Validation e a = Failure e | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure x = Success x
  (Failure e1) <*> (Failure e2) = Failure (e1 <> e2)
  (Failure e)  <*> (Success _)  = Failure e
  (Success _)  <*> (Failure e)  = Failure e
  (Success f)  <*> (Success x)  = Success (f x)
