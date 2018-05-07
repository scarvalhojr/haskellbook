
-- Compile with:
--   ghc -prof -fprof-auto -rtsopts -O2 section28.3.hs
-- Run with:
--   ./section28.3 +RTS -p

f :: IO ()
f = do
  print ([1..] !! 999999)
  putStrLn "f"

g :: IO ()
g = do
  print ([1..] !! 9999999)
  putStrLn "g"

main :: IO ()
main = do
  f
  g
