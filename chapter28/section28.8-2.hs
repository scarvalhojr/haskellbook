
import           Criterion.Main
import qualified Data.Vector    as V (Vector, fromList, map)

-- Compile with -O2 to benefit from loop fusion; with loop fusion, prefused
-- map functions will perform almost as well as a series of map operations

testPrefusedVecMaps :: Int -> V.Vector Int
testPrefusedVecMaps n = V.map ((+n) . (+n) . (+n) . (+n)) vec
  where vec = V.fromList [1..10000]

testVecMaps :: Int -> V.Vector Int
testVecMaps n = V.map (+n) $ V.map (+n) $ V.map (+n) $ V.map (+n) vec
  where vec = V.fromList [1..10000]

testPrefusedListMaps :: Int -> [Int]
testPrefusedListMaps n = map ((+n) . (+n) . (+n) . (+n)) [1..10000]

testListMaps :: Int -> [Int]
testListMaps n = map (+n) $ map (+n) $ map (+n) $ map (+n) [1..10000]

main :: IO ()
main = defaultMain
  [ bench "vector maps prefused" $
      whnf testPrefusedVecMaps 9998
  , bench "vector maps will be fused" $
      whnf testVecMaps 9998
  , bench "list maps prefused" $
      whnf testPrefusedListMaps 9998
  , bench "list maps will also be fused" $
      whnf testListMaps 9998
  ]
