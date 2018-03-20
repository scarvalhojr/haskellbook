
-- import Control.Monad
import qualified Control.Monad.Trans.State as S (State, get, put, execState)
import qualified Data.DList                as D (DList, empty, snoc)

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

main1 :: IO ()
main1 = mapM_ (putStrLn . fizzBuzz) [1..100]

---

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = S.execState (mapM_ addResult list) []

addResult :: Integer -> S.State [String] ()
addResult n = do
  xs <- S.get
  let result = fizzBuzz n
  S.put (result : xs)

main2 :: IO ()
main2 = mapM_ putStrLn $ reverse (fizzbuzzList [1..100])

---

fizzbuzzListD :: [Integer] -> D.DList String
fizzbuzzListD list = S.execState (mapM_ addResultD list) D.empty

addResultD :: Integer -> S.State (D.DList String) ()
addResultD n = do
  xs <- S.get
  let result = fizzBuzz n
  S.put (D.snoc xs result)

main3 :: IO ()
main3 = mapM_ putStrLn $ fizzbuzzListD [1..100]

---

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = S.execState (mapM_ addResult list) []
  where list = [to,to - 1..from]

main4 :: IO ()
main4 = mapM_ putStrLn $ fizzbuzzFromTo 1 100
