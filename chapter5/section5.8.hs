{-# LANGUAGE NoMonomorphismRestriction #-}

example1 = (* 9) 6
example2 = head [(0, "dodge"), (1, "kitteh")]
example3 = head [(0 :: Integer, "dodge"), (1, "kitteh")]
example4 = if False then True else False
example5 = length [1, 2, 3, 4, 5]
example6 = (length [1, 2, 3, 4]) > (length "TACOCAT")
