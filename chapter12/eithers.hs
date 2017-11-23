
myLefts :: [Either a b] -> [a]
myLefts = foldr takeLeft []
  where takeLeft (Left x)  acc = x : acc
        takeLeft (Right _) acc = acc

myRights :: [Either a b] -> [b]
myRights = foldr takeRight []
  where takeRight (Left _)  acc = acc
        takeRight (Right x) acc = x : acc

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr part ([], [])
  where part (Left x)  (ls, rs) = (x:ls, rs)
        part (Right x) (ls, rs) = (ls, x:rs)

eitherMaybe :: (b -> c) -> Either a b -> Maybe c
eitherMaybe _ (Left _)  = Nothing
eitherMaybe f (Right x) = Just (f x)

myEither :: (a -> c) -> (b -> c) -> Either a b -> c
myEither lf _  (Left l)  = lf l
myEither _  rf (Right r) = rf r

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f = myEither (\_ -> Nothing) (\x -> Just (f x))
