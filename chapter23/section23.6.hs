
newtype Moi s a = Moi { runMoi :: s -> (a, s) }


instance Functor (Moi s) where
  -- fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (x, s') = g s in (f x, s')


instance Applicative (Moi s) where
  -- pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  -- (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> let (x, s')  = g s
                                        (h, s'') = f s'
                                     in (h x, s'')

instance Monad (Moi s) where
  return = pure

  -- (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s -> let (x, s') = f s in runMoi (g x) s'
