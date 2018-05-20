
import Control.Exception (SomeException(..), catch)
import Data.Typeable     (typeOf)

handler :: SomeException -> IO ()
handler (SomeException e) = do
  print (typeOf e)
  putStrLn ("We errored! It was: " ++ show e)
  writeFile "bbb" "hi"
  putStrLn "Trying to write to bbb instead"

main = do
  putStrLn "Trying to write to zzz..."
  writeFile "zzz" "hi" `catch` handler
  putStrLn "Done!"
