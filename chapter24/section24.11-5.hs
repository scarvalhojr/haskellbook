
import Control.Applicative     ((<|>), (<*), (*>))
import Control.Monad           (void)
import Text.Printf             (printf)
import Text.Parser.Char        (char, anyChar, newline, space, spaces, string)
import Text.Parser.Token       (decimal)
import Text.Parser.Combinators (skipSome, skipMany, some, many, manyTill, eof, try)
import Text.Trifecta           (Parser, Result(..), parseString)

type Year = Integer
type Month = Integer
type Day = Integer
type Hour = Integer
type Minute = Integer
type Description = String

data Date = Date Year Month Day
  deriving (Eq, Ord)

instance Show Date where
  show (Date year month day) = printf "%04d" year ++ "-" ++
                               printf "%02d" month ++ "-" ++
                               printf "%02d" day

data Time = Time Hour Minute
  deriving (Eq, Ord)

instance Show Time where
  show (Time hour min) = printf "%02d" hour ++ ":" ++ printf "%02d" min

data Activity = Activity Time Description

instance Show Activity where
  show (Activity time desc) = show time ++ " " ++ desc

data DailyLog = DailyLog Date [Activity]

instance Show DailyLog where
  show (DailyLog date activities) = unlines $ ("# " ++ show date) :
                                              (map show activities)

---

skipComment :: Parser ()
skipComment = char '-' *> char '-' *> skipRestOfLine

skipRestOfLine :: Parser ()
skipRestOfLine = void $ manyTill anyChar ((void newline) <|> eof)

skipSpaceAndComments :: Parser ()
skipSpaceAndComments = skipMany (skipComment <|> skipSome space)

parseDate :: Parser Date
parseDate = Date <$> (decimal <* char '-') <*> (decimal <* char '-') <*> decimal

parseDateLine :: Parser Date
parseDateLine = char '#' *> char ' ' *> parseDate <* skipRestOfLine

parseTime :: Parser Time
parseTime = Time <$> (decimal <* char ':') <*> decimal

parseDescription :: Parser Description
parseDescription = manyTill anyChar (try skipComment <|> (void newline) <|> eof)

parseActivityLine :: Parser Activity
parseActivityLine = Activity <$> parseTime <*> (char ' ' *> parseDescription)

parseActivities :: Parser [Activity]
parseActivities = some (skipSpaceAndComments *> parseActivityLine <* skipSpaceAndComments)

parseDay :: Parser DailyLog
parseDay = do
  skipSpaceAndComments
  date <- parseDateLine
  activities <- parseActivities
  return $ DailyLog date activities

parseLog :: Parser [DailyLog]
parseLog = some parseDay

---

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

runLogParser :: String -> Maybe [DailyLog]
runLogParser = maybeSuccess . parseString parseLog mempty

sampleLog :: String
sampleLog = "\
\  \t  \n\
\-- wheee a comment\n\
\    \n\
\   -- another comment \n\
\# 2025-02-05\n\
\  \t \n\
\08:00 Breakfast\n\
\ \n\
\   09:00 Sanitizing moisture collector -- comment here too\n\
\11:00 Exercising in high-grav gym\n\
\12:00 Lunch\n\
\13:00 Programming\n\
\17:00 Commuting home in rover\n\
\17:30 R&R\n\
\19:00 Dinner\n\
\21:00 Shower\n\
\21:15 Read\n\
\22:00 Sleep\n\
\\n\
\# 2025-02-07 -- dates not nececessarily sequential\n\
\08:00 Breakfast -- should I try skippin bfast?\n\
\09:00 Bumped head, passed out\n\
\13:36 Wake up, headache\n\
\13:37 Go to medbay\n\
\13:40 Patch self up\n\
\13:45 Commute home for rest\n\
\14:15 Read\n\
\21:00 Dinner\n\
\21:15 Read\n\
\22:00 Sleep\n\
\# 2025-02-08\n\
\08:00 Stay in bed"
