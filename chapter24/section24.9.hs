
{-# LANGUAGE OverloadedStrings #-}

-- import Control.Applicative
import Data.Ratio            ((%))
import Data.String           (IsString)
import Data.Attoparsec.Text  (parseOnly)
import Text.Trifecta


badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

main :: IO ()
main = do
  putStrLn "Parsing with Attoparsec"
  let attoP = parseOnly parseFraction
  print $ attoP badFraction
  print $ attoP shouldWork
  print $ attoP shouldAlsoWork
  print $ attoP alsoBad

  putStrLn "Parsing with Trifecta"
  let triP = parseString parseFraction mempty
  print $ triP badFraction
  print $ triP shouldWork
  print $ triP shouldAlsoWork
  print $ triP alsoBad
