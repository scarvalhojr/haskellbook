
import Text.Parser.Char        (char, digit)
import Text.Parser.Combinators (count, some, try)
import Control.Applicative     ((<|>), (*>))
import Text.Trifecta           (Parser, parseString)

---

eightDigits :: Parser [Char]
eightDigits = count 8 digit

someDigits :: Parser [Char]
someDigits = some digit

twoOrThreeDigits :: Parser [Char]
twoOrThreeDigits = try (count 3 digit) <|> count 2 digit

---

-- https://stackoverflow.com/q/49751139/8254774

zeroUpTo :: Int -> Parser a -> Parser [a]
zeroUpTo n p
  | n > 0      = (:) <$> try p <*> zeroUpTo (n - 1) p <|> return []
  | otherwise  = return []

oneUpTo :: Int -> Parser a -> Parser [a]
oneUpTo n p
  | n > 0      = (:) <$> p <*> zeroUpTo (n - 1) p
  | otherwise  = return []
