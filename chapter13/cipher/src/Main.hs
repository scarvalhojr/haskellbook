
module Main where

import System.IO
import System.Exit (exitSuccess)
import Control.Monad (forever)
import Data.Char (toLower)
import Cipher (encrypt, decrypt)

data Choice = Encrypt | Decrypt | Quit

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  forever $ do
    choice <- getChoice
    case choice of
      Encrypt -> runEncrypt
      Decrypt -> runDecrypt
      Quit    -> exitSuccess

getChoice :: IO (Choice)
getChoice = do
  putStr $ "Press 'E' to encrypt, 'D' to decrypt, or 'Q' to quit: "
  choice <- getLine
  case map toLower choice of
    ('e':_) -> return Encrypt
    ('d':_) -> return Decrypt
    ('q':_) -> return Quit
    _       -> do putStrLn "Invalid choice."
                  getChoice

runEncrypt :: IO ()
runEncrypt = do
  putStr $ "Enter the encryption key: "
  key <- getLine
  putStr $ "Enter the plain text: "
  plaintext <- getLine
  let encrypted = encrypt plaintext key
  putStrLn $ "Encrypted text: " ++ encrypted

runDecrypt :: IO ()
runDecrypt = do
  putStr $ "Enter the decryption key: "
  key <- getLine
  putStr $ "Enter the encrypted text: "
  encrypted <- getLine
  let plaintext = decrypt encrypted key
  putStrLn $ "Plain text: " ++ plaintext
