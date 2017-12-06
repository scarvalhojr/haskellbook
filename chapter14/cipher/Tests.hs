
module Tests where

import Cipher (encrypt, decrypt)
import Test.QuickCheck

prop_DecryptEncrypt :: String -> String -> Bool
prop_DecryptEncrypt text key = decrypt (encrypt text key) key == text

main :: IO ()
main = do quickCheck prop_DecryptEncrypt
