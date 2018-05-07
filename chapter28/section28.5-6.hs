
import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

-- 28.5 Maps

genList :: Int -> [(String, Int)]
genList n = go n []
  where go 0  xs = ("0", 0) : xs
        go n' xs = go (n' - 1) ((show n', n') : xs)

pairList :: [(String, Int)]
pairList = genList 9001

testMap :: M.Map String Int
testMap = M.fromList pairList

-- 28.6 Sets

bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

main :: IO ()
main = defaultMain
  [ bench "lookup one thing - list" $
      whnf (lookup "doesntExist") pairList
  , bench "lookup one thing - map" $
      whnf (M.lookup "doesntExist") testMap
  , bench "member check - map" $
      whnf membersMap 9999
  , bench "member check - set" $
      whnf membersSet 9999
  , bench "member insert - map" $
      whnf (M.insert 9999 9999) m
  , bench "member insert - set" $
      whnf (S.insert 9999) s
  , bench "member delete - map" $
      whnf (M.delete 9999) m
  , bench "member delete - set" $
      whnf (S.delete 9999) s
  , bench "union - map" $
      whnf (M.union m) m
  , bench "union - set" $
      whnf (S.union s) s
  ]
