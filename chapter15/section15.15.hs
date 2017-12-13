
import Data.Monoid (Monoid, mappend, mempty)
import Data.Semigroup (Semigroup, Sum(..), (<>))
import Test.QuickCheck (quickCheck, oneof, Arbitrary(arbitrary), Gen, sample, CoArbitrary)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

---

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

---

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityTest = Identity [Int]

---

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoTest = Two String [Int]

---

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeTest = Three String [Float] (Sum Int)

---

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Semigroup (Four a b c d) where
    (Four a b c d) <> (Four a' b' c' d') =
      Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return $ Four a b c d

type FourTest = Four String [Float] (Sum Int) (Either Double Char)

---

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Monoid (BoolConj) where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return $ BoolConj b

---

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Arbitrary BoolDisj where
  arbitrary = do
    b <- arbitrary
    return $ BoolDisj b

---

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd x) <> _ = Snd x
  _       <> x = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Fst a, return $ Snd b]

type OrTest = Or Char Int

---

newtype Combine a b = Combine { unCombine :: (a -> b)}

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> (f x) <> (g x))

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine (\x -> mempty)
  mappend = (<>)

instance Show (Combine a b) where
  show _ = "Combine a b"

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = fmap Combine arbitrary

combineSemigroupAssoc :: (Eq b, Show b, Semigroup b)
                      => a
                      -> Combine a b
                      -> Combine a b
                      -> Combine a b
                      -> Bool
combineSemigroupAssoc x (Combine f) (Combine g) (Combine h) =
  (f x <> (g x <> h x)) == ((f x <> g x) <> h x)

type CombineTest = Combine String [Int]

f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)

res1 :: Sum Int
res1 = unCombine (f <> g) $ 0

res2 :: Sum Int
res2 = unCombine (mappend f mempty) $ 1

---

newtype Comp a = Comp { unComp :: (a -> a)}

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

f' = Comp $ \(Sum n) -> Sum (n + 1)
g' = Comp $ \(Sum n) -> Sum (n - 1)

res1' :: Sum Int
res1' = unComp (f' <> g') $ Sum 0

res2' :: Sum Int
res2' = unComp (mappend f' mempty) $ Sum 0

---

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Failure e1 <> Failure e2 = Failure (e1 <> e2)
  Failure _  <> Success a  = Success a
  Success a  <> _          = Success a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Failure a, return $ Success b]

type ValidationTest = Validation String Int

---

newtype AccumulateRight a b = AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Success x) <> AccumulateRight (Success y) = AccumulateRight (Success (x <> y))
  AccumulateRight (Success x) <> _                           = AccumulateRight (Success x)
  _                           <> y                           = y


instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ AccumulateRight (Failure a), return $ AccumulateRight (Success b)]

type AccumulateRightTest = AccumulateRight String [Int]

---

newtype AccumulateBoth a b = AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Failure x) <> AccumulateBoth (Failure y) = AccumulateBoth (Failure (x <> y))
  AccumulateBoth (Success x) <> AccumulateBoth (Success y) = AccumulateBoth (Success (x <> y))
  AccumulateBoth (Failure x) <> _                          = AccumulateBoth (Failure x)
  _                          <> AccumulateBoth (Failure y) = AccumulateBoth (Failure y)


instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ AccumulateBoth (Failure a), return $ AccumulateBoth (Success b)]

type AccumulateBothTest = AccumulateBoth String [Int]

---

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdentityTest -> IdentityTest -> IdentityTest -> Bool)
  quickCheck (monoidLeftIdentity :: IdentityTest -> Bool)
  quickCheck (monoidRightIdentity :: IdentityTest -> Bool)
  quickCheck (semigroupAssoc :: TwoTest -> TwoTest -> TwoTest -> Bool)
  quickCheck (monoidLeftIdentity :: TwoTest -> Bool)
  quickCheck (monoidRightIdentity :: TwoTest -> Bool)
  quickCheck (semigroupAssoc :: ThreeTest -> ThreeTest -> ThreeTest -> Bool)
  quickCheck (semigroupAssoc :: FourTest -> FourTest -> FourTest -> Bool)
  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  quickCheck (semigroupAssoc :: OrTest -> OrTest -> OrTest -> Bool)
  quickCheck (combineSemigroupAssoc :: String -> CombineTest -> CombineTest -> CombineTest -> Bool)
  quickCheck (semigroupAssoc :: ValidationTest -> ValidationTest -> ValidationTest -> Bool)
  quickCheck (semigroupAssoc :: AccumulateRightTest -> AccumulateRightTest -> AccumulateRightTest -> Bool)
  quickCheck (semigroupAssoc :: AccumulateBothTest -> AccumulateBothTest -> AccumulateBothTest -> Bool)
