
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import           Control.Applicative         ((<|>))
import           Data.Text                   (Text)
import           Data.Scientific             (floatingOrInteger)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy as LBS (ByteString)

newtype Host = Host String
  deriving (Eq, Show)

instance FromJSON Host where
  parseJSON (Object v) = Host <$> v .: "host"
  parseJSON _          = fail "Expected an object for Host"

type Annotation = String

data Color = Red Annotation
           | Blue Annotation
           | Yellow Annotation
  deriving (Eq, Show)

instance FromJSON Color where
  parseJSON (Object v) = (Red <$> v .: "red") <|>
                         (Blue <$> v .: "blue") <|>
                         (Yellow <$> v .: "yellow")
  parseJSON _          = fail "Expected an object for Color"

data TestData = TestData { section :: Host
                         , what    :: Color }
  deriving (Eq, Show)

instance FromJSON TestData where
  parseJSON (Object v) = TestData <$> v .: "section" <*> v .: "whatisit"
  parseJSON _          = fail "Expected an object for TestData"

sectionJson :: ByteString
sectionJson = "{ \"section\": {\"host\": \"wikipedia.org\"},\n\
              \  \"whatisit\": {\"red\": \"intoothandclaw\"}\n\
              \}\n"

---

data NumberOrString = Numba Integer | Stringy Text
  deriving (Eq, Show)

instance FromJSON NumberOrString where
  parseJSON (Number i) =
    case floatingOrInteger i of
      (Left _)  -> fail "Must be integral number"
      (Right x) -> return $ Numba x
  parseJSON (String s) = return $ Stringy s
  parseJSON _          = fail "NumberOrString must be number or string"

dec :: LBS.ByteString -> Maybe NumberOrString
dec = decode

eitherDec :: LBS.ByteString -> Either String NumberOrString
eitherDec = eitherDecode

---

main = do
  let blah :: Maybe TestData
      blah = decodeStrict sectionJson
  print blah
  print $ dec "123"
  print $ eitherDec "\"blah\""
