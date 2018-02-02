
import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

identityTest :: [Int] -> Bool
identityTest x = functorIdentity x

---

functorComposition :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorComposition f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

compositionTest :: [Int] -> Bool
compositionTest x = functorComposition (+1) (*2) x

---

functorComposition' :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorComposition' (Fun _ f) (Fun _ g) x = (fmap g (fmap f x)) == (fmap (g . f) x)

type IntToInt = Fun Int Int
type IntFunctorComposition = IntToInt -> IntToInt -> [Int] -> Bool

---

test :: IO ()
test = do
  quickCheck identityTest
  quickCheck compositionTest
  quickCheck (functorComposition' :: IntFunctorComposition)
