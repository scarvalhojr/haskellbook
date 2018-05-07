
import Criterion.Main


data Queue a = Queue { enqueue :: [a], dequeue :: [a] }
  deriving (Eq, Show)

empty :: Queue a
empty = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue e d) = Queue (x : e) d

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []     ) = Nothing
pop (Queue e  (x : d)) = Just (x, Queue e d)
pop (Queue e  []     ) = Just (x, Queue [] d)
  where r = reverse e
        x = head r
        d = tail r

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

---

bigQueue :: Queue Int
bigQueue = testPushQueue 100000

testPushQueue :: Int -> Queue Int
testPushQueue n = foldr (\x q -> push x q) empty [1..n]

testPopQueue :: Queue Int -> Maybe Int
testPopQueue q
  | isEmpty q  = Nothing
  | isEmpty q' = Just x
  | otherwise  = testPopQueue q'
  where Just (x, q') = pop q

---

bigList = [1..100000]

testPushList :: Int -> [Int]
testPushList n = foldr (\x q -> q ++ [x]) [] [1..n]

testPopList :: [Int] -> Maybe Int
testPopList []     = Nothing
testPopList [x]    = Just x
testPopList (x:xs) = testPopList xs

---

main :: IO ()
main = defaultMain
  [ bench "push queue" $ whnf testPushQueue 100000
  , bench "push list"  $ whnf testPushList 100000
  , bench "pop queue"  $ whnf testPopQueue bigQueue
  , bench "pop list"  $ whnf testPopList bigList
  ]
