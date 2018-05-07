
import           Criterion.Main
import           Control.Monad.Primitive           (RealWorld)
import           Control.Monad.ST                  (runST)
import qualified Data.Vector                 as V  (Vector, freeze)
import qualified Data.Vector.Mutable         as MV (MVector, write)
import qualified Data.Vector.Generic.Mutable as GM (new)

mutableUpdateIO :: Int -> IO (MV.MVector RealWorld Int)
mutableUpdateIO n = do
  mvec <- GM.new (n + 1)
  go n mvec
  where go 0 v = return v
        go n v = (MV.write v n 0) >> go (n - 1) v

mutableUpdateST :: Int -> V.Vector Int
mutableUpdateST n = runST $ do
  mvec <- GM.new (n + 1)
  go n mvec
  where go 0 v = V.freeze v
        go n v = (MV.write v n 0) >> go (n - 1) v

main :: IO ()
main = defaultMain
  [ bench "mutable IO vector" $ whnfIO (mutableUpdateIO 9998)
  , bench "mutable ST vector" $ whnf mutableUpdateST 9998
  ]
