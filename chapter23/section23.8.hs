
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (x, s') = g s in (f x, s')

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (State f) <*> (State g) = State $ \s -> let (x, s')  = g s
                                              (h, s'') = f s'
                                           in (h x, s'')

instance Monad (State s) where
  return = pure
  (State f) >>= g = State $ \s -> let (x, s') = f s in runState (g x) s'

-- 1.

get :: State s s
get = State $ \s -> (s, s)


-- 2.

put :: s -> State s ()
put s = State $ const ((), s)

-- 3.

exec :: State s a -> s -> s
exec (State sa) s = let (_, s') = sa s in s'

-- 4.

eval :: State s a -> s -> a
eval (State sa) s = let (x, _) = sa s in x

-- 5.

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)
