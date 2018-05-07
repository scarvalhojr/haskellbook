
import           Criterion.Main
import qualified Data.Vector         as B (Vector, fromList, foldr)
import qualified Data.Vector.Unboxed as U (Vector, fromList, foldr)

-- Compile with:
--   ghc -prof -rtsopts -O2 section28.8-5.hs
-- Run with:
--   ./section28.8-5 +RTS -hc
-- Produce heap usage graph with:
--   hp2ps -c section28.8-5.hp

{-# SCC boxedVec #-}
{-# SCC unboxedVec #-}

boxedVec :: B.Vector Int
boxedVec = B.fromList [0..100000]

boxedFold :: B.Vector Int -> Int
boxedFold = B.foldr (\n z -> (n + z) `div` z) 1

unboxedVec :: U.Vector Int
unboxedVec = U.fromList [0..100000]

unboxedFold :: U.Vector Int -> Int
unboxedFold = U.foldr (\n z -> (n + z) `div` z) 1

main :: IO ()
main = defaultMain
  [ bench "boxed fold" $ nf boxedFold boxedVec
  , bench "unboxed fold" $ nf unboxedFold unboxedVec
  ]
