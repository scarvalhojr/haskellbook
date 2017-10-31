
-- Types

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

main = do
  putStrLn $ appedCatty "woohoo!"
  putStrLn $ frappe "1"
  putStrLn $ frappe (appedCatty "2")
  putStrLn $ appedCatty (frappe "blue")
  putStrLn $ cattyConny (frappe "pink")
                        (cattyConny "green" (appedCatty "blue"))
  putStrLn $ cattyConny (flippy "Pugs" "are") "awesome"

-- Recursion

sumAll :: (Eq a, Num a) => a -> a
sumAll 0 = 0
sumAll n = n + sumAll (n - 1)

mult :: Integral a => a -> a -> a
mult m n
  | m == 0    = 0
  | m < 0     = mult (-m) (-n)
  | otherwise = n + mult (m - 1) n

-- Fixing dividedBy

data DividedResult = Result Integer | DividedByZero deriving Show

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denom = go num denom 0 1
  where go n d count sign
          | n < 0     = go (-n) d count (-sign)
          | d < 0     = go n (-d) count (-sign)
          | n < d     = Result (sign * count)
          | otherwise = go (n - d) d (count + 1) sign

-- McCarthy 91

mc91 :: Integral a => a -> a
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 (mc91 (n + 11))
