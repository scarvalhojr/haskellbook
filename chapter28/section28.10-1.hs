
import Criterion.Main


newtype DList a = DL { unDL :: [a] -> [a] }

-- 1.

empty :: DList a
empty = DL id
{-# INLINE empty #-}

-- 2.

singleton :: a -> DList a
singleton = DL . (:)
{-# INLINE singleton #-}

-- 3.

toList :: DList a -> [a]
toList = ($[]) . unDL
{-# INLINE toList #-}

-- 4. Prepend a single element to a dlist.

infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

-- 5. Append a single element to a dlist.

infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x:))
{-# INLINE snoc #-}

-- 6. Append dlists.

append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)
{-# INLINE append #-}

---

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

consDlist :: Int -> [Int]
consDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (cons n xs)

snocDlist :: Int -> [Int]
snocDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (snoc xs n)

appendDlist :: Int -> [Int]
appendDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (singleton n `append` xs)

main :: IO ()
main = defaultMain
  [ bench "concat list" $ whnf schlemiel 1234567
  , bench "cons dlist" $ whnf consDlist 1234567
  , bench "snoc dlist" $ whnf snocDlist 1234567
  , bench "append dlist" $ whnf appendDlist 1234567
  ]
