
{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad             (liftM)

-- 1.

newtype IdentityT f a = IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance MonadTrans IdentityT where
  lift :: Monad m => m a -> IdentityT m a
  lift = IdentityT

-- 2.

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift = MaybeT . liftM Just

-- 3.

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance MonadTrans (ReaderT r) where
  lift :: Monad m => m a -> ReaderT r m a
  lift = ReaderT . const

-- 4.

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift = EitherT . fmap Right

-- 5.

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = StateT $ \s -> do
    a <- m
    return $ (a, s) 
