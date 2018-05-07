
import qualified Data.Text             as T    (pack, unpack)
import qualified Data.Text.Encoding    as TE   (encodeUtf8, decodeUtf8)
import qualified Data.ByteString       as B    (ByteString)
import qualified Data.ByteString.Char8 as B8   (pack)
import qualified Data.ByteString.UTF8  as UTF8 (fromString)

-- Manual unicode encoding of Japanese text
-- GHC Haskell allows UTF8 in source files
s :: String
s = "\12371\12435\12395\12385\12399\12289\
\20803\27671\12391\12377\12363\65311"

utf8ThenPrint :: B.ByteString -> IO ()
utf8ThenPrint = putStrLn . T.unpack . TE.decodeUtf8

-- this function throws an exception:
-- Cannot decode byte '\x93': Data.Text.Internal.Encoding.decodeUtf8:
-- Invalid UTF-8 stream
throwsException :: IO ()
throwsException = utf8ThenPrint (B8.pack s)

bytesByWayOfText :: B.ByteString
bytesByWayOfText = TE.encodeUtf8 (T.pack s)

-- letting utf8-string do it for us
libraryDoesTheWork :: B.ByteString
libraryDoesTheWork = UTF8.fromString s

thisWorks :: IO ()
thisWorks = utf8ThenPrint bytesByWayOfText

alsoWorks :: IO ()
alsoWorks = utf8ThenPrint libraryDoesTheWork

main :: IO ()
main = do
  -- thisWorks is equivalent to:
  putStrLn $ (T.unpack . TE.decodeUtf8 . TE.encodeUtf8 . T.pack) s
  -- alsoWorks is equivalent to:
  putStrLn $ (T.unpack . TE.decodeUtf8 . UTF8.fromString) s
  -- throwsException is equivalent to:
  putStrLn $ (T.unpack . TE.decodeUtf8 . B8.pack) s
