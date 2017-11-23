
myIterate :: (a -> a) -> a -> [a]
myIterate f z = z : (myIterate f (f z))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f z = case f z of Nothing     -> []
                            Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))
