{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

--instance TooMany Goats where
--  tooMany (Goats n) = n > 43

newtype NumAndText = NAT (Int, String)

instance TooMany NumAndText where
  tooMany (NAT (n, _)) = n > 100

instance TooMany (Int, String) where
  tooMany (n, _) = n > 10

--instance TooMany (Int, Int) where
--  tooMany (n, m) = (n + m) > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, m) = tooMany n || tooMany m
