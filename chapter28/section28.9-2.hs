
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO           as TIO (putStrLn)
import qualified Data.Text.Encoding     as TE  (decodeUtf8)
import qualified Data.ByteString.Lazy   as BL  (ByteString, toStrict)
import qualified Codec.Compression.GZip as GZ  (compress)


input :: BL.ByteString
input = "123"

compressed :: BL.ByteString
compressed = GZ.compress input

main :: IO ()
main = do
  TIO.putStrLn $ TE.decodeUtf8 (BL.toStrict input)
  -- the next line will fail because there’ll be a byte that isn’t recognizably
  -- correct as an encoding of text information
  TIO.putStrLn $ TE.decodeUtf8 (BL.toStrict compressed)
