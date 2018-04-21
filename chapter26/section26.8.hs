
import Control.Monad.Trans.Maybe  (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)

-- newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
-- newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

unwrapped :: IO (Either String (Maybe Int))
unwrapped = readerUnwrap ()

-- unwrapped = Right (Just 1)

embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = MaybeT $ ExceptT $ ReaderT $ (return .) (const (Right (Just 1)))

unwrapped' :: IO (Either String (Maybe Int))
unwrapped' = runReaderT (runExceptT (runMaybeT embedded')) ()

-- unwrapped' = Right (Just 1)
