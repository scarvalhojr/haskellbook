
import Text.Trifecta           (Parser, unexpected, char, parseString, choice,
                                string)
import Text.Parser.Combinators (eof)
import Control.Applicative     ((<|>))

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'

-- 1.

oneEOF = one >> eof

test1 = do
  pNL "oneEOF succeeds parsing '1':"
  print $ parseString oneEOF mempty "1"
  pNL "oneEOF fails parsing '12':"
  print $ parseString oneEOF mempty "12"

-- 2.

multString :: Parser ()
multString = (string "123" <|> string "12" <|> string "1") >> eof

test2 = do
  pNL "multString succeeds parsing '1':"
  print $ parseString multString mempty "1"
  pNL "multString succeeds parsing '12':"
  print $ parseString multString mempty "12"
  pNL "multString succeeds parsing '123':"
  print $ parseString multString mempty "123"
  pNL "multString fails parsing '124':"
  print $ parseString multString mempty "124"
  pNL "multString fails parsing '1234':"
  print $ parseString multString mempty "1234"

-- 3.

stringAsChar :: Parser ()
stringAsChar = char 'w' >> char 'h' >> char 'a' >> char 't' >> eof
