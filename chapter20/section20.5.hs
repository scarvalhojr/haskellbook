
import Data.Monoid (Sum(..), Product(..), Any(..), All(..), (<>))


sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

sum'' :: (Foldable t, Num a) => t a -> a
sum'' = getSum . foldMap Sum

---

product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

product'' :: (Foldable t, Num a) => t a -> a
product'' = getProduct . foldMap Product

---

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e = foldr (\x acc -> acc || x == e) False

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' e = getAny . foldMap (Any . (== e))

---

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr fmin Nothing
  where fmin x Nothing  = Just x
        fmin x (Just y) = Just (min x y)

---

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr fmax Nothing
  where fmax x Nothing  = Just x
        fmax x (Just y) = Just (max x y)

---

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

null'' :: (Foldable t) => t a -> Bool
null'' = getAll . foldMap (\_ -> All False)

---

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ acc -> acc + 1) 0

length'' :: (Foldable t) => t a -> Int
length'' = getSum . foldMap (\_ -> Sum 1)

---

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\x acc -> x : acc) []

toList'' :: (Foldable t) => t a -> [a]
toList'' = foldMap (\x -> [x])

---

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap (id)

---

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x acc -> (f x) <> acc) mempty
