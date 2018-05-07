
incdInts :: [Integer] -> [Integer]
incdInts x = map (+1) x

-- Compile with:
--   ghc -prof -fprof-auto -rtsopts -O2 section28.4-2.hs
-- Run with:
--   ./section28.4-2 +RTS -hc -p
-- Produce heap usage graph with:
--   hp2ps section28.4-2.hp

main :: IO ()
main = do
  print (incdInts [1..] !! 1000)
  print (incdInts [1..] !! 9001)
  print (incdInts [1..] !! 90010)
  print (incdInts [1..] !! 9001000)
  print (incdInts [1..] !! 9501000)
  print (incdInts [1..] !! 9901000)
