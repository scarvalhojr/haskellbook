
{-# LANGUAGE InstanceSigs #-}

import Control.Monad.IO.Class    (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad             (liftM)

-- 1.

newtype IdentityT m a = IdentityT { runIdentityT :: m a }
  deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

instance MonadTrans IdentityT where
  lift :: Monad m => m a -> IdentityT m a
  lift = IdentityT

instance (MonadIO m) => MonadIO (IdentityT m) where
  liftIO :: IO a -> IdentityT m a
  liftIO = lift . liftIO

-- 2.

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT meea) = EitherT $ (fmap . fmap) f meea

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . Right
  (EitherT emab) <*> (EitherT ema) = EitherT $ (<*>) <$> emab <*> ema

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT meea) >>= f = EitherT $ do
    v <- meea
    case v of
      Right r -> runEitherT (f r)
      Left l  -> return (Left l)

instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift = EitherT . fmap Right

instance (MonadIO m) => MonadIO (EitherT e m) where
  liftIO :: IO a -> EitherT e m a
  liftIO = lift . liftIO

-- 1.

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  MaybeT mmab <*> MaybeT mma = MaybeT $ (<*>) <$> mmab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (MaybeT mma) >>= f = MaybeT $ do
    v <- mma
    case v of
      Nothing -> return Nothing
      Just x  -> runMaybeT (f x)

instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift = MaybeT . liftM Just

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  liftIO = lift . liftIO

-- 2.

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  ReaderT rmab <*> ReaderT rma = ReaderT $ (<*>) <$> rmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

instance MonadTrans (ReaderT r) where
  lift :: Monad m => m a -> ReaderT r m a
  lift = ReaderT . const

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO = lift . liftIO

-- 3.

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT smas) = StateT $ (fmap . fmap) f' smas
    where f' (a, s) = (f a, s)

instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  StateT smab <*> StateT sma = StateT $ \s -> do
    (f, s') <- smab s
    (a, s'')<- sma s'
    return $ (f a, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  StateT sma >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = StateT $ \s -> do
    a <- m
    return $ (a, s)

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO :: IO a -> StateT s m a
  liftIO = lift . liftIO
