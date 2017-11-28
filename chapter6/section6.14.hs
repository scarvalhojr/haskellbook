
import Data.List (sort)

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

---

data Mood = Blah | Woot deriving (Eq, Show)

settleDown x = if x == Woot then Blah else x

---

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool"

s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"

---

data Rocks = Rocks String deriving (Eq, Ord, Show)

data Yeah = Yeah Bool deriving (Eq, Ord, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Ord, Show)

phew = Papu (Rocks "chases") (Yeah True)

truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

---

i :: Num a => a

-- This fails:
-- i :: a
i = 1

--f :: Float

-- This fails:
-- f :: Num a => a

-- This is okay:
-- f :: Fractional a => a

-- This is okay:
f :: RealFrac a => a
f = 1.0

-- freud :: a -> a

-- This is okay:
freud :: Ord a => a -> a
freud x = x

-- freud' :: a -> a

-- This is okay:
freud' :: Int -> Int
freud' x = x

myX = 1 :: Int

sigmund :: Int -> Int
-- This fails:
-- sigmund :: a -> a
sigmund x = myX

sigmund' :: Int -> Int
-- This fails:
-- sigmund' :: Num a => a -> a
sigmund' x = myX

-- jung :: Ord a => [a] -> a

-- This is okay:
jung :: [Int] -> Int
jung xs = head (sort xs)

-- young :: [Char] -> Char
-- This is okay:
young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- This fails:
--signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

---

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = (f x) == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x y = (fromInteger x) + (f y)
