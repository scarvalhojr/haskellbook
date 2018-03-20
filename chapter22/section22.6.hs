
-- 1.

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

-- 2.

asks :: (r -> a) -> Reader r a
asks f = Reader f

-- 3.

newtype Reader r a = Reader { runReader:: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure x  = Reader (const x)

  (Reader rab) <*> (Reader ra) = Reader (\x -> rab x (ra x))
