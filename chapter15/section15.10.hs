
data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty  = Nada
  mappend m        Nada     = m
  mappend Nada     m        = m
  mappend (Only x) (Only y) = Only (mappend x y)
