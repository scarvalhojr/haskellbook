module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO

newtype WordList = WordList [String] deriving (Eq, Show)

data Puzzle = Puzzle String [Maybe Char] [Char] deriving Eq

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " # Guessed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '-'
renderPuzzleChar (Just c) = c

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

maxIncorrectGuesses :: Int
maxIncorrectGuesses = 4

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  allWords <- loadWords
  putStrLn $ "You can have " ++ (show maxIncorrectGuesses) ++ " incorrect guesses."
  word <- randomWord allWords
  runGame (newPuzzle (map toLower word))

loadWords :: IO WordList
loadWords = do
  dict <- readFile "data/dict.txt"
  let ws = filter validLength (lines dict)
  putStrLn $ "Loaded " ++ (show (length ws)) ++ " words."
  return $ WordList ws
  where validLength w =
          let len = length w
          in  len >= minWordLength && len <= maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  rnd <- randomRIO (0, (length wl) - 1)
  return $ wl !! rnd

newPuzzle :: String -> Puzzle
newPuzzle w = Puzzle w (map (const Nothing) w) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) = (`elem` w)

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) = (`elem` g)

incorrectGuesses :: Puzzle -> Int
incorrectGuesses (Puzzle w _ g) = length $ filter (not . (`elem` w)) g

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter p@(Puzzle word _ guess) c
  | elem c guess  = p
  | otherwise     = Puzzle word disc' guess'
  where guess' = c : guess
        disc' = map (\x -> if elem x guess' then Just x else Nothing) word

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: '" ++ [guess] ++ "'"
  if alreadyGuessed puzzle guess then
    do putStrLn "You already guess that letter, try again."
       return puzzle
  else
    do if charInWord puzzle guess then
         do putStrLn "Nice one!"
       else
         do putStrLn "Not so lucky this time."
       return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver p@(Puzzle word _ _) =
  if incorrectGuesses p > maxIncorrectGuesses then
    do putStrLn $ "Game over! The word was '" ++ word ++ "'."
       exitSuccess
  else
    return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word disc _) =
  if all isJust disc then
    do putStrLn $ "You won! The word was '" ++ word ++ "'."
       exitSuccess
  else
    return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character."
