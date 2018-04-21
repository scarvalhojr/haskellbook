
{-# LANGUAGE InstanceSigs #-}

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma

instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure = MaybeT . pure . pure

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  MaybeT mmab <*> MaybeT mma = MaybeT $ (<*>) <$> mmab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT mma) >>= f = MaybeT $ do
    v <- mma
    case v of
      Nothing -> return Nothing
      Just x  -> runMaybeT (f x)

---

x :: MaybeT [] Char
x = MaybeT [Just 'a', Nothing, Just ' ']

f :: Char -> MaybeT [] String
f ' ' = MaybeT [Nothing]
f x   = MaybeT [Just (x:[x])]

y :: MaybeT [] String
y = x >>= f

z :: [Maybe String]
z = runMaybeT y

-- z = [Just "aa",Nothing,Nothing]
