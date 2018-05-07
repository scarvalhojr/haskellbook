
incdInts :: [Integer]
incdInts = map (+1) [1..]

-- Compile with:
--   ghc -prof -fprof-auto -rtsopts -O2 section28.4.hs
-- Run with:
--   ./section28.4 +RTS -hc -p
-- Produce heap usage graph with:
--   hp2ps section28.4.hp

main :: IO ()
main = do
  print (incdInts !! 1000)
  print (incdInts !! 9001)
  print (incdInts !! 90010)
  print (incdInts !! 9001000)
  print (incdInts !! 9501000)
  print (incdInts !! 9901000)
