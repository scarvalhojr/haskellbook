{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map            as M (fromList, insert, empty)
import           Data.Map                 (Map)
import           Control.Applicative      ((<|>), some)
import           Data.ByteString          (ByteString)
import           Text.Trifecta
import           Test.Hspec

newtype Header = Header String
  deriving (Eq, Ord, Show)

type Name = String
type Value = String
type Assignments = Map Name Value

data Section = Section Header Assignments
  deriving (Eq, Show)

newtype Config = Config (Map Header Assignments)
  deriving (Eq, Show)

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  skipEOL
  return (name, val)

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assigns <- some parseAssignment
  return $ Section h (M.fromList assigns)

skipComments :: Parser ()
skipComments = skipMany skipComment
  where skipComment = do _ <- char ';' <|> char '#'
                         skipMany (noneOf "\n")
                         skipEOL

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n' <|> char '\t')

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  return $ Config (foldr rollup M.empty sections)

---

headerSample1 :: ByteString
headerSample1 = "[blah]"

assignmentSample1 :: ByteString
assignmentSample1 = "woot=1"

commentSample1 :: ByteString
commentSample1 = "; last modified 1 April 2001 by Someone"

commentSample2 :: ByteString
commentSample2 = "; blah\n; woot\n  \n;hah"

sectionSample1 :: ByteString
sectionSample1 = "; ignore me\n[states]\nChris=Texas"

sectionSample2 :: ByteString
sectionSample2 = "\n; ignore me\n[states]\nChris=Texas\n"

sectionSample3 :: ByteString
sectionSample3 = "\n; comment\n[section]\nhost=wikipedia.org\nalias=claw\n\n\
                 \[whatisit]\nred=intoothandclaw\n"

runParser :: Parser a -> ByteString -> Maybe a
runParser p = maybeSuccess . parseByteString p mempty

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do

  describe "Header parsing" $
    it "can parse a simple header" $ do
      let result = runParser parseHeader headerSample1
      result `shouldBe` Just (Header "blah")

  describe "Assignment parsing" $
    it "can parse a simple assignment" $ do
      let result = runParser parseAssignment assignmentSample1
      result `shouldBe` Just ("woot", "1")

  describe "Comment parsing" $
    it "skips comment before header" $ do
      let result = runParser (skipComments >> parseHeader) "; woot\n[blah]"
      result `shouldBe` Just (Header "blah")

  describe "Section parsing" $
    it "can parse a simple section" $ do
      let result = runParser parseSection sectionSample1
          states = M.fromList [("Chris", "Texas")]
          expected = Just (Section (Header "states") states)
      result `shouldBe` expected

  describe "INI parsing" $
    it "Can parse multiple sections" $ do
      let result  = runParser parseIni sectionSample3
          assign1 = M.fromList [("alias", "claw")
                                     ,("host", "wikipedia.org")]
          assign2 = M.fromList [("red", "intoothandclaw")]
          expected = Just (Config (M.fromList [(Header "section", assign1)
                                              ,(Header "whatisit", assign2)]))
      result `shouldBe` expected
