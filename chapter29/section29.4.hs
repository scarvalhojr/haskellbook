
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)

myData :: IO (MVar Int)
myData = newEmptyMVar

main :: IO ()
main = do
  mv <- myData
  mv' <- myData
  putMVar mv 1
  putMVar mv' 2
  mvVal' <- takeMVar mv'
  mvVal <- takeMVar mv
  putStrLn $ "mvVal = " ++ show mvVal
  putStrLn $ "mvVal' = " ++ show mvVal'
