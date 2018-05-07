
import           Criterion.Main
import           Data.Vector      ((//))
import qualified Data.Vector as V (Vector, fromList, unsafeUpdate)

-- Compile with -O2 to get the most benefit from faster vector updates; without
-- compiler optimizations, batch updates with a vector argument may not be
-- slower than batch updates with a list argument

vec :: V.Vector Int
vec = V.fromList [1..10000]

slow :: Int -> V.Vector Int
slow n = go n vec
  where go 0 v = v
        go n v = go (n - 1) (v // [(n, 0)])

batchList :: Int -> V.Vector Int
batchList n = vec // updates
  where updates = fmap (\n -> (n, 0)) [0..n]

batchVector :: Int -> V.Vector Int
batchVector n = V.unsafeUpdate vec updates
  where updates = fmap (\n -> (n, 0)) (V.fromList [0..n])

main :: IO ()
main = defaultMain
  [ bench "slow" $ whnf slow 9998
  , bench "batch list" $ whnf batchList 9998
  , bench "batch vector" $ whnf batchVector 9998
  ]
