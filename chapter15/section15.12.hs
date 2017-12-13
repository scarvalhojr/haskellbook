
import Control.Monad
import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

---

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools), (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

testBullMonoid :: IO ()
testBullMonoid = do
  quickCheck (monoidAssoc :: Bull -> Bull -> Bull -> Bool)
  -- This does not hold:
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  -- This does not hold:
  quickCheck (monoidRightIdentity :: Bull -> Bool)

--

data Optional a = Nada | Only a deriving (Eq, Show)

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty  = First' Nada
  mappend (First' Nada) x = x
  mappend x             _ = x

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    oneof [return $ First' Nada,
           return $ First' (Only a)]

testFirstMonoid :: IO ()
testFirstMonoid = do
  quickCheck (monoidAssoc :: First' String -> First' String -> First' String -> Bool)
  quickCheck (monoidLeftIdentity :: First' Int -> Bool)
  quickCheck (monoidRightIdentity :: First' Char -> Bool)

-- print some sample arbirtrary values
-- sample (arbitrary :: Gen (First' String))
