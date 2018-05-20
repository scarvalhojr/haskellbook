
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import Options.Generic
import Cipher          (encrypt, decrypt)

data CmdOption = Encrypt { key :: String, timeout :: Maybe Int }
               | Decrypt { key :: String, timeout :: Maybe Int }
  deriving (Generic, Show)

modifiers :: Modifiers
modifiers = defaultModifiers { shortNameModifier = firstLetter }

instance ParseRecord CmdOption where
  parseRecord = parseRecordWithModifiers modifiers

defaultTimeoutSec :: Int
defaultTimeoutSec = 10

main :: IO ()
main = do
  opt <- getRecord "Vigenère Cipher"

  -- Time out if input isn't available
  let timeout = getTimeout opt defaultTimeoutSec
  hSetBuffering stdin NoBuffering
  available <- hWaitForInput stdin (timeout * 1000)

  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering

  if available then
    case opt of
      Encrypt key _ -> interact (encrypt key)
      Decrypt key _ -> interact (decrypt key)
  else
    putStrLn $ "Time out reached: " ++ show timeout ++ " seconds."

getTimeout :: CmdOption -> Int -> Int
getTimeout (Encrypt _ (Just t)) _ = t
getTimeout (Decrypt _ (Just t)) _ = t
getTimeout _                    d = d
