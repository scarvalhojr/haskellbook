
import Criterion.Main
import qualified Data.Sequence as S

-- Compile with -O2 to get the most benefit from sequences

lists :: [[Int]]
lists = replicate 10 [1..100000]

seqs :: [S.Seq Int]
seqs = replicate 10 (S.fromList [1..100000])

---

myList :: [Int]
myList = [1..100000]

mySeq :: S.Seq Int
mySeq = S.fromList [1..100000]

---

main :: IO ()
main = defaultMain
  [ bench "concatenate lists" $
      nf mconcat lists
  , bench "concatenate sequences" $
      nf mconcat seqs
  , bench "indexing list" $
      whnf (\xs -> xs !! 9001) myList
  , bench "indexing sequence" $
      whnf (flip S.index 9001) mySeq
  ]
