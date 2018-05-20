
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock    (getCurrentTime, utctDay)
import System.Random      (randomIO)

huehue :: IO (Either (IO Int) (IO ()))
huehue = do
  t <- getCurrentTime
  let (_, _, dayOfMonth) = toGregorian (utctDay t)
  case even dayOfMonth of
    True  -> return $ Left randomIO
    False -> return $ Right (putStrLn "no soup for you")
