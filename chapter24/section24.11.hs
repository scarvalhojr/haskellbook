
import Data.Char               (digitToInt)
import Control.Applicative     ((<|>), (<*), (*>))
import Text.Parser.Char        (char, oneOf)
import Text.Parser.Combinators (some)
import Text.Trifecta           (Parser, parseString)

-- 2.

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

-- 3.

base10Integer' :: Parser Integer
base10Integer' = (char '-' *> (negate <$> base10Integer)) <|> base10Integer

-- 4.

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = undefined
