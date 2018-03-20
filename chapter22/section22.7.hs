
newtype HumanName = HumanName String
  deriving (Eq, Show)

newtype DogName = DogName String
  deriving (Eq, Show)

newtype Address = Address String
  deriving (Eq, Show)

data Person = Person { humanName :: HumanName
                     , dogName :: DogName
                     , address :: Address } deriving (Eq, Show)

data Dog = Dog { dogsName :: DogName
               , dogsAddress :: Address } deriving (Eq, Show)

getDogMonad :: Person -> Dog
getDogMonad = do
  name <- dogName
  addr <- address
  return (Dog name addr)

---

sergio :: Person
sergio = Person (HumanName "Sergio") (DogName "None") (Address "Dublin")

--- 1.

newtype Reader r a = Reader { runReader:: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure x  = Reader (const x)

  (Reader rab) <*> (Reader ra) = Reader (\x -> rab x (ra x))

instance Monad (Reader r) where
  return = pure

  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

-- 2.

-- ???
getDogReader :: Person -> Dog
getDogReader p = undefined
