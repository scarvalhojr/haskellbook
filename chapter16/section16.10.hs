
import Test.QuickCheck (quickCheck, Arbitrary(..))
import Test.QuickCheck.Function (Fun(..))

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorComposition :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorComposition (Fun _ f) (Fun _ g) x
  = (fmap g (fmap f x)) == (fmap (g . f) x)

-- type IntToInt = Fun Int Int
type IntToChar = Fun Int Char
type CharToBool = Fun Char Bool

---

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityTest = Identity Int

---

data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return $ Pair a1 a2

type PairTest = Pair Int

---

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoTest = Two Char Int

---

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeTest = Three Bool Char Int

---

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return $ Three' a b1 b2

type Three'Test = Three' Char Int

---

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d)
  where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return $ Four a b c d

type FourTest = Four Bool String Char Int

---

data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
      a1 <- arbitrary
      a2 <- arbitrary
      a3 <- arbitrary
      b <- arbitrary
      return $ Four' a1 a2 a3 b

type Four'Test = Four' Char Int

---

data Trivial = Trivial

-- Not possible to implement Functor since its kind is * (and not * -> *)

---

test :: IO ()
test = do
  quickCheck (functorIdentity :: IdentityTest -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> IdentityTest -> Bool)
  quickCheck (functorIdentity :: PairTest -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> PairTest -> Bool)
  quickCheck (functorIdentity :: TwoTest -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> TwoTest -> Bool)
  quickCheck (functorIdentity :: ThreeTest -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> ThreeTest -> Bool)
  quickCheck (functorIdentity :: Three'Test -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> Three'Test -> Bool)
  quickCheck (functorIdentity :: FourTest -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> FourTest -> Bool)
  quickCheck (functorIdentity :: Four'Test -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> Four'Test -> Bool)
