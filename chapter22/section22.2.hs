
import Control.Applicative (liftA2)
import Data.Char (toUpper)


boop :: Num a => a -> a
boop = (*2)

doop :: Num a => a -> a
doop = (+10)

bip :: Num a => a -> a
bip = boop . doop

bloop :: Num a => a -> a
bloop = fmap boop doop

bbop :: Num a => a -> a
bbop = (+) <$> boop <*> doop

duwop :: Num a => a -> a
duwop = liftA2 (+) boop doop

boopDoop :: Num a => a -> a
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

---

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

doed :: [Char] -> ([Char], [Char])
doed = do
  a <- cap
  b <- rev
  return (a, b)

bound :: [Char] -> ([Char], [Char])
bound = cap >>= (\x -> rev >>= (\y -> return (x, y)))
