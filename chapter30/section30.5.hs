
import Control.Exception
import Control.Concurrent (threadDelay)
import Control.Monad      (forever)
import System.Random      (randomRIO)

canICatch :: Exception e => e -> IO (Either ArithException ())
canICatch e = try $ throwIO e

catchAnything :: Exception e => e -> IO (Either SomeException ())
catchAnything e = try $ throwIO e

---

randomException :: IO ()
randomException = do
  i <- randomRIO (1, 10 :: Int)
  if i `elem` [1..9] then
    throwIO DivideByZero
  else
    throwIO StackOverflow

main :: IO ()
main = forever $ do
  let tryS :: IO () -> IO (Either ArithException ())
      tryS = try
  _ <- tryS randomException
  putStrLn "Live to loop another day!"
  -- microseconds
  threadDelay (1 * 1000000)
