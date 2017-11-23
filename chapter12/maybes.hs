
isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just x) = True

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

myMaybe :: b -> (a -> b) -> Maybe a -> b
myMaybe z _ Nothing  = z
myMaybe _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe z Nothing  = z
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes []           = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs)  = x : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe []           = Just []
flipMaybe (Nothing:xs) = Nothing
flipMaybe (Just x:xs)  = case flipMaybe xs of Nothing -> Nothing
                                              Just l  -> Just (x:l)
