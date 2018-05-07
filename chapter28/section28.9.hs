
import           Criterion.Main
import           Control.Monad             (replicateM_)
import qualified System.IO         as SIO  (readFile)
import qualified Data.Text         as T    (Text)
import qualified Data.Text.Lazy    as TL   (Text)
import qualified Data.Text.IO      as TIO  (readFile, putStrLn)
import qualified Data.Text.Lazy.IO as TLIO (readFile, putStrLn)

-- Compile with:
--   ghc -prof -rtsopts -O2 section28.9.hs
-- Run with:
--   ./section28.9 +RTS -hc > /dev/null
-- Produce heap usage graph with:
--   hp2ps -c section28.9.hp

dictFilePath :: String
dictFilePath = "/usr/share/dict/words"

dictString :: IO String
dictString = SIO.readFile dictFilePath

printDictString :: Int -> IO ()
printDictString n = replicateM_ n (dictString >>= print)

dictText :: IO T.Text
dictText = TIO.readFile dictFilePath

printDictText :: Int -> IO ()
printDictText n = replicateM_ n (dictText >>= TIO.putStrLn)

dictTextLazy :: IO TL.Text
dictTextLazy = TLIO.readFile dictFilePath

printDictTextLazy :: Int -> IO ()
printDictTextLazy n = replicateM_ n (dictTextLazy >>= TLIO.putStrLn)

main :: IO ()
main = do
  printDictString 100
  printDictText 100
  printDictTextLazy 100
