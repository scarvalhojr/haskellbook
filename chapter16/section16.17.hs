
{-# LANGUAGE FlexibleInstances #-}

import GHC.Arr (Array)

---

data Bool'' = False'' | True''

-- No Functor instance because kind is *

---

data BoolAndSomethingElse a = False' a | True' a
  deriving Show

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True'  a) = True'  (f a)

---

data BoolAndMaybeSomethingElse a = Falsish | Truish a
  deriving Show

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish    = Falsish
  fmap f (Truish a) = Truish (f a)

---

newtype Mu f = InF { outF :: f (Mu f) }

-- No Functor? Kind is: (* -> *) -> *

---

data D = D (Array Word Word) Int Int

-- No Functor? Kind is: *̀

---

data Sum b a = First a | Second b
  deriving Show

instance Functor (Sum e) where
  fmap _ (Second b) = Second b
  fmap f (First  a) = First (f a)

---

data Company a c b = DeepBlue a c | Something b
  deriving Show

instance Functor (Company e e') where
  fmap _ (DeepBlue a c) = DeepBlue a c
  fmap f (Something b)  = Something (f b)

---

data More b a = L a b a | R b a b
  deriving Show

instance Functor (More b) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

---

data Quant a b = Finance | Desk a | Bloor b
  deriving Show

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)

---

data K a b = K a
  deriving Show

instance Functor (K a) where
  fmap _ (K a) = K a

---

newtype Flip f a b = Flip (f b a)
  deriving (Eq, Show)

newtype K' a b = K' a
  deriving Show

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip (K' (f a))

---

data EvilGoateeConst a b = GoatyConst b
  deriving Show

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

---

data LiftItOut f a = LiftItOut (f a)
  deriving Show

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

---

data Parappa f g a = DaWrappa (f a) (g a)
  deriving Show

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

--    > fmap (+1) (DaWrappa (Just 6) (Nothing))
--    DaWrappa (Just 7) Nothing
--    > fmap (+1) (DaWrappa (Just 6) (Left "err"))
--    DaWrappa (Just 7) (Left "err")
--    > fmap (+1) (DaWrappa (Just 6) (Right 10.8))
--    DaWrappa (Just 7.0) (Right 11.8)

---

data IgnoreOne f g a b = IgnoreSomething (f a) (g b)
  deriving Show

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (fmap f gb)

---

data Notorious g o a t = Notorious (g o) (g a) (g t)
  deriving Show

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

---

data List a = Nil | Cons a (List a)
  deriving Show

instance Functor List where
  fmap _ Nil        = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

--    > fmap (+1) (Cons 2 (Cons 1 (Cons 0 Nil)))
--    Cons 3 (Cons 2 (Cons 1 Nil))

---

data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
                deriving Show

instance Functor GoatLord where
  fmap _ NoGoat               = NoGoat
  fmap f (OneGoat a)          = OneGoat (f a)
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

---

data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt         = Halt
  fmap f (Print s a)  = Print s (f a)
  fmap f (Read sa)    = Read (\s -> f (sa s))

  -- alternatively...
  -- fmap f (Read sa) = Read (fmap f sa)
