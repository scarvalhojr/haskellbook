
import Control.Exception

main :: IO ()
main = do
  throwIO DivideByZero
  putStrLn "lol"

-- This also works
main' :: IO ()
main' = do
  throw DivideByZero
  putStrLn "lol"
