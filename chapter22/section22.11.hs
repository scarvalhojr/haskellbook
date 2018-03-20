
import Control.Applicative (liftA2)
import Data.Maybe (fromMaybe)
import Data.Monoid (All(..))

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- it's also nice to have one that
-- will return Nothing, like this one
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a
-- variable lookup key
z' :: Integer -> Maybe Integer
z' = flip lookup (zip x z)

-- Have x1 make a tuple of xs and ys
x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

-- And x2 make a tuple of ys and zs
x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

-- Write x3 which takes one input and makes a tuple of the results of two
-- applications of z' from above.
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

-- summed is uncurry with addition as
-- the first argument
summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  putStrLn $ "Ex 1:"
  print $ (getAll . foldMap All . sequA) 4
  print $ and (sequA 4)
  putStrLn $ "Ex 2:"
  print $ sequA (fromMaybe 0 s')
  putStrLn $ "Ex 3:"
  print $ bolt (fromMaybe 0 ys)
