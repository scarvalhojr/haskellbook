
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import System.IO.Unsafe   (unsafePerformIO)

myData :: MVar Int
myData = unsafePerformIO newEmptyMVar

main :: IO ()
main = do
  putMVar myData 1
  val <- takeMVar myData
  print val
  putMVar myData 2
  val <- takeMVar myData
  print val
