
{-# LANGUAGE InstanceSigs #-}

import Data.Char (intToDigit, isHexDigit)

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

-- 1.

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT smas) = StateT $ (fmap . fmap) f' smas
    where f' (a, s) = (f a, s)

-- 2.

instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \s -> pure (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT smab <*> StateT sma = StateT $ \s -> do
    (f, s') <- smab s
    (a, s'')<- sma s'
    return $ (f a, s'')

-- 3.

instance (Monad m) => Monad (StateT s m) where
  return :: a -> StateT s m a
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT sma >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'

---

x :: StateT Int Maybe Char
x = StateT $ \s -> if s > 15 then Nothing else Just (intToDigit s, s + 1)

f :: Char -> String
f = replicate 2

y :: StateT Int Maybe String
y = fmap f x

y' :: [Maybe (String, Int)]
y' = fmap (runStateT y) [0..17]

-- y' = [Just ("00",1),Just ("11",2),Just ("22",3),Just ("33",4),Just ("44",5),
--       Just ("55",6),Just ("66",7),Just ("77",8),Just ("88",9),Just ("99",10),
--       Just ("aa",11),Just ("bb",12),Just ("cc",13),Just ("dd",14),
--       Just ("ee",15),Just ("ff",16),Nothing,Nothing]

--

f2 :: StateT Int Maybe (Char -> String)
f2 = StateT $ \s -> if s > 15 then Nothing else Just (f, s)

y2 :: StateT Int Maybe String
y2 = f2 <*> x

y2' :: [Maybe (String, Int)]
y2' = fmap (runStateT y2) [0..17]

-- y2' == y'
-- True

--

f3 :: Char -> StateT Int Maybe String
f3 x
  | isHexDigit x  = StateT $ \s -> Just (f x, s)
  | otherwise     = StateT $ \x -> Nothing

y3 :: StateT Int Maybe String
y3 = x >>= f3

y3' :: [Maybe (String, Int)]
y3' = fmap (runStateT y3) [0..17]

-- y3' == y2'
-- True
