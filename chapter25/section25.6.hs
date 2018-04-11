
{-# LANGUAGE InstanceSigs #-}

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- 1.

data Deux a b = Deux a b
  deriving (Eq, Show)

instance Bifunctor Deux where
  bimap :: (a -> b) -> (c -> d) -> Deux a c -> Deux b d
  bimap f g (Deux a b) = Deux (f a) (g b)

  first :: (a -> b) -> Deux a c -> Deux b c
  first f (Deux a c) = Deux (f a) c

  second :: (b -> c) -> Deux a b -> Deux a c
  second f (Deux a b) = Deux a (f b)

-- 2.

data Const a b = Const a
  deriving (Eq, Show)

instance Bifunctor Const where
  bimap :: (a -> b) -> (c -> d) -> Const a c -> Const b d
  bimap f _ (Const a) = Const (f a)

-- 3.

data Drei a b c = Drei a b c
  deriving (Eq, Show)

instance Bifunctor (Drei a) where
  bimap :: (b -> c) -> (d -> e) -> Drei a b d -> Drei a c e
  bimap f g (Drei a b d) = Drei a (f b) (g d)

-- 4.

data SuperDrei a b c = SuperDrei a b
  deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
  bimap :: (b -> c) -> (d -> e) -> SuperDrei a b d -> SuperDrei a c e
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

-- 5.

data SemiDrei a b c = SemiDrei a
  deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where
  bimap :: (b -> c) -> (d -> e) -> SemiDrei a b d -> SemiDrei a c e
  bimap _ _ (SemiDrei a) = SemiDrei a

-- 6.

data Quadriceps a b c d = Quadzzz a b c d
  deriving (Eq, Show)

instance Bifunctor (Quadriceps a b) where
  bimap :: (c -> d) -> (e -> f) -> Quadriceps a b c e -> Quadriceps a b d f
  bimap g h (Quadzzz a b c e) = Quadzzz a b (g c) (h e)

-- 7.

data MyEither a b = MyLeft a | MyRight b
  deriving (Eq, Show)

instance Bifunctor MyEither where
  bimap :: (a -> b) -> (c -> d) -> MyEither a c -> MyEither b d
  bimap f _ (MyLeft a)  = MyLeft (f a)
  bimap _ g (MyRight a) = MyRight (g a)
