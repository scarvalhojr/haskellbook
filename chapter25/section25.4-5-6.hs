
{-# LANGUAGE InstanceSigs #-}

newtype Identity a = Identity { runIdentity :: a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure :: a -> Identity a
  pure = Identity

  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  return :: a -> Identity a
  return = pure

  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  Identity x >>= f = f x

instance Foldable Identity where
  foldMap :: Monoid m => (a -> m) -> Identity a -> m
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse :: Functor f => (a -> f b) -> Identity a -> f (Identity b)
  traverse f (Identity x) = Identity <$> (f x)

---

newtype Compose f g a = Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . (pure . pure)

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  Compose fgab <*> Compose fga = Compose $ (fmap (<*>) fgab) <*> fga

-- Impossible
-- instance (Monad f, Monad g) => Monad (Compose f g) where
--   return :: a -> Compose f g a
--   return = pure
--   (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
--   Compose fga >>= f = f a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative h => (a -> h b) -> Compose f g a -> h (Compose f g b)
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga
