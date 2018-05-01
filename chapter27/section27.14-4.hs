
{-# LANGUAGE BangPatterns #-}

-- Using only bang patterns or seq, make the code bottom out
-- when executed.

x = undefined
y = "blah"

-- No bottom
main = do
  print (snd (x, y))

-- Bottom
main1 = do
  print $ f (x, y)
  where f (!x, y) = snd (x, y)

-- Bottom
main2 = do
  print $ x `seq` (snd (x, y))
