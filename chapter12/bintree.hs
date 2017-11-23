
data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f z = case f z of Nothing        -> Leaf
                         Just (l, m, r) -> Node (unfold f l) m (unfold f r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold build n
  where build x
          | x <= 0    = Nothing
          | otherwise = Just (x - 1, x - 1, x -1)
