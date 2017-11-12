
import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [ DbDate (UTCTime (fromGregorian 1911 5 1)
                              (secondsToDiffTime 34123)),
                DbNumber 9001,
                DbString "Hello, world!",
                DbNumber 578,
                DbDate (UTCTime (fromGregorian 1921 5 1)
                                (secondsToDiffTime 34123))]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr onlyDbDate []
  where onlyDbDate (DbDate x) xs = x : xs
        onlyDbDate _ xs          = xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr onlyDbNumber []
  where onlyDbNumber (DbNumber x) xs = x : xs
        onlyDbNumber _ xs          = xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr maxDbDate zeroDate
  where maxDbDate (DbDate x) y = max x y
        maxDbDate _ y          = y
        zeroDate = UTCTime (fromGregorian 0 1 1) (secondsToDiffTime 0)

sumDbNumber :: [DatabaseItem] -> Integer
sumDbNumber = foldr addDbNumber 0
  where addDbNumber (DbNumber x) acc = x + acc
        addDbNumber _ acc            = acc

avgDbNumber :: [DatabaseItem] -> Double
avgDbNumber = (\(a, c) -> (fromInteger a) / (fromInteger c)) . (foldr addDbNumber (0, 0))
  where addDbNumber (DbNumber x) (acc, count) = (acc + x, count + 1)
        addDbNumber _            (acc, count) = (acc, count)
