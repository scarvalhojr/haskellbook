
import Control.Monad.Trans.Maybe (MaybeT(..))

fileSize :: FilePath -> MaybeT IO Integer
fileSize "file1" = MaybeT $ return (Just 10)
fileSize "file2" = MaybeT $ return (Just 20)
fileSize _       = MaybeT $ return Nothing

addT :: FilePath -> FilePath -> IO (Maybe Integer)
addT f1 f2 = runMaybeT $ do
  s1 <- fileSize f1
  s2 <- fileSize f2
  return (s1 + s2)
