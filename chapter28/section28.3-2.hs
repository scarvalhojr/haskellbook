
import Control.Monad

-- Compile with:
--   ghc -prof -fprof-auto -rtsopts -O2 section28.3-2.hs
-- Run with:
--   ./section28.3-2 +RTS -hc -p > /dev/null
-- Produce heap usage graph with:
--   hp2ps -c section28.3-2.hp

blah :: [Integer]
blah = [1..1000]

main :: IO ()
main = replicateM_ 10000 (print blah)
