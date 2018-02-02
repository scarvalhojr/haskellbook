
import Test.Hspec
import Data.Monoid ((<>))


data Nope a = NopeDotJpg
  deriving Show

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure x = NopeDotJpg

  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure

  _ >>= _ = NopeDotJpg

---

data MyEither b a = MyLeft a | MyRight b
  deriving Show

instance Functor (MyEither b) where
  fmap _ (MyRight x) = MyRight x
  fmap f (MyLeft x)  = MyLeft (f x)

instance Applicative (MyEither b) where
  pure x = MyLeft x

  MyRight x <*> _         = MyRight x
  _         <*> MyRight x = MyRight x
  MyLeft f  <*> MyLeft x  = MyLeft (f x)

instance Monad (MyEither b) where
  return = pure

  MyLeft x  >>= f = f x
  MyRight x >>= _ = MyRight x

---

data Identity a = Identity a
  deriving Show

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure x = Identity x

  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  return = pure

  Identity x >>= f = f x

---

data List a = Nil | Cons a (List a)
  deriving Show

instance Monoid (List a) where
  mempty = Nil

  mappend Nil         ys = ys
  mappend (Cons x xs) ys = Cons x (xs <> ys)

instance Functor List where
  fmap _ Nil        = Nil
  fmap f (Cons h t) = Cons (f h) (f <$> t)

instance Applicative List where
  pure x = Cons x Nil

  Nil       <*> _   = Nil
  _         <*> Nil = Nil
  Cons f fs <*> xs  = (fmap f xs) <> (fs <*> xs)

instance Monad List where
  return = pure

  Nil      >>= _ = Nil
  Cons x t >>= f = (f x) <> (t >>= f)

---

j :: Monad m => m (m a) -> m a
j x = x >>= id

---

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

---

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = (fmap f x) <*> y

-- l2 = liftA2   -- import Control.Applicative (liftA2)

---

a :: Monad m => m a -> m (a -> b) -> m b
a x f = f <*> x

-- a = flip ap   -- import Control.Monad (ap)

---

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh []     _ = return []
meh (x:xs) f = (fmap (:) (f x)) <*> (meh xs f)

---

flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id

---

test :: IO ()
test = hspec $ do

  describe "j" $ do
    it "works as expected" $ do
      j [[1, 2], [], [3]]                `shouldBe` [1, 2, 3]
      j (Just (Just 1))                  `shouldBe` Just 1
      j (Just (Nothing :: Maybe Char))   `shouldBe` Nothing
      j (Nothing :: Maybe (Maybe Float)) `shouldBe` Nothing

  describe "l1" $ do
    it "works as expected" $ do
      l1 length (Just [1..10])         `shouldBe` Just 10
      l1 length (Nothing :: Maybe [a]) `shouldBe` Nothing

  describe "l2" $ do
    it "works as expected" $ do
      let f c r = take r (repeat c)
      l2 f (Just 'x') (Just 5)              `shouldBe` Just "xxxxx"
      l2 f (Just 'x') Nothing               `shouldBe` Nothing
      l2 f (Nothing :: Maybe Char) (Just 5) `shouldBe` Nothing

  describe "a" $ do
    it "works as expected" $ do
      let f x = take x (repeat '.')
      a (Just 5) (Just f)                          `shouldBe` Just "....."
      a (Just 5) (Nothing :: Maybe (Int -> Float)) `shouldBe` Nothing
      a Nothing  (Just f)                          `shouldBe` Nothing

  describe "meh" $ do
    it "works as expected" $ do
      let f x = if even x then Just (take x (repeat '.')) else Nothing
      meh [2,4]   f `shouldBe` Just ["..", "...."]
      meh [2,4,1] f `shouldBe` Nothing

  describe "flipType" $ do
    it "works as expected" $ do
      flipType [Just 5, Just 10] `shouldBe` Just [5, 10]
      flipType [Just 5, Nothing] `shouldBe` Nothing
