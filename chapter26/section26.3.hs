
{-# LANGUAGE InstanceSigs #-}

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-- 1. Write the Functor instance for EitherT:

instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT meea) = EitherT $ (fmap . fmap) f meea

-- 2. Write the Applicative instance for EitherT:

instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure = EitherT . pure . Right

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT emab) <*> (EitherT ema) = EitherT $ (<*>) <$> emab <*> ema

-- 3. Write the Monad instance for EitherT:

instance Monad m => Monad (EitherT e m) where
  return :: a -> EitherT e m a
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT meea) >>= f = EitherT $ do
    v <- meea
    case v of
      Right r -> runEitherT (f r)
      Left l  -> return (Left l)

-- 4. Write the swapEitherT helper function for EitherT:

swapEither :: Either a b -> Either b a
swapEither (Left a)  = Right a
swapEither (Right b) = Left b

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ fmap swapEither ema

-- 5. Write the transformer variant of the either catamorphism:

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb (EitherT amb) = amb >>= f
  where f (Left a)  = fa a
        f (Right b) = fb b

---

x :: EitherT String [] Int
x = EitherT [Left "Oops", Right 0, Left "Ouch", Right 3]

f1 :: Int -> [Int]
f1 x = replicate x x

x1 :: EitherT String [] [Int]
x1 = fmap f1 x

x1' :: [Either String [Int]]
x1' = runEitherT x1

-- x1' = [Left "Oops",Right [],Left "Ouch",Right [3,3,3]]

f2 :: EitherT String [] (Int -> [Int])
f2 = EitherT [Left "No!", Right f1, Left "Boom"]

x2 :: EitherT String [] [Int]
x2 = f2 <*> x

x2' :: [Either String [Int]]
x2' = runEitherT x2

-- x2' = [Left "No!",Left "No!",Left "No!",Left "No!",
--        Left "Oops",Right [],Left "Ouch",Right [3,3,3],
--        Left "Boom",Left "Boom",Left "Boom",Left "Boom"]

f3 :: Int -> EitherT String [] [Int]
f3 0 = EitherT [Left "Zero", Left "Nada"]
f3 x = EitherT [Right (f1 x), Right (f1 (x + 1))]

x3 :: EitherT String [] [Int]
x3 = x >>= f3

x3' :: [Either String [Int]]
x3' = runEitherT x3

-- x3' = [Left "Oops",
--        Left "Zero",Left "Nada",
--        Left "Ouch",
--        Right [3,3,3],Right [4,4,4,4]]

x4 :: EitherT [Int] [] String
x4 = swapEitherT x3

x4' :: [Either [Int] String]
x4' = runEitherT x4

-- x4' = [Right "Oops",
--        Right "Zero",Right "Nada",
--        Right "Ouch",
--        Left [3,3,3],Left [4,4,4,4]]

f5a :: [Int] -> [String]
f5a = map show

f5b :: String -> [String]
f5b = (:[])

x5 :: [String]
x5 = eitherT f5a f5b x4

-- x5 = ["Oops",
--       "Zero","Nada",
--       "Ouch",
--       "3","3","3","4","4","4","4"]
