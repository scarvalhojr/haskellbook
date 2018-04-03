
import Data.Ratio          ((%))
import Control.Applicative ((<|>), some, many)
import Text.Trifecta       (Parser, Result, integer, letter, parseString, oneOf,
                            skipMany, token, char, decimal, try)

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNoS :: Parser NumberOrString
parseNoS = (Left <$> integer) <|> (Right <$> some letter)

runParser :: Parser a -> String -> Result a
runParser parser str = parseString parser mempty str

test1 = do
  print $ runParser (some letter) a
  print $ runParser integer b
  print $ runParser parseNoS a
  print $ runParser parseNoS b
  print $ runParser (many parseNoS) c
  print $ runParser (some parseNoS) c

---

eitherOr :: String
eitherOr = "\n123\nabc\n456\ndef\n"

parseNoS' :: Parser NumberOrString
parseNoS' =
  skipMany (oneOf "\n") >>
  (Left <$> integer) <|> (Right <$> some letter)

test2 = do
  print $ runParser (some (token parseNoS')) eitherOr

---

type DecOrFrac = Either Integer Rational

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

parseDecimal :: Parser Integer
parseDecimal = decimal

parseDecOrFrac :: Parser DecOrFrac
parseDecOrFrac = (try (Right <$> parseFraction)) <|> (Left <$> parseDecimal)

test3 = do
  print $ runParser parseDecOrFrac "1"
  print $ runParser parseDecOrFrac "1/2"
