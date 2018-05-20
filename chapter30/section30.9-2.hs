
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import System.IO

data PleaseDie = PleaseDie
  deriving Show

instance Exception PleaseDie

openAndWrite :: IO ()
openAndWrite = do
  h <- openFile "test.dat" WriteMode
  threadDelay 1500
  hPutStr h (replicate 100000000 '0' ++ "abc")
  hClose h

main :: IO ()
main = do
  threadId <- forkIO (mask_ openAndWrite)
  threadDelay 10000
  throwTo threadId PleaseDie
  putStrLn "Maind thread done"
