module Common
    ( Choice
    , Input(..)
    , Player(..)
    , GameT
    , runGame
    , playHand
    , reportScore
    , reportWinner
    , readInput
    ) where


import qualified Data.Map        as M (Map, empty, findWithDefault, alter)
import Data.Char                      (isDigit)
import Control.Monad.Trans.State      (StateT, evalStateT, get, modify)
import Control.Monad.IO.Class         (liftIO)


type Choice = Integer

data Input = Play Choice | Quit

data Player = Human | Computer | Player1 | Player2
  deriving (Eq, Ord, Show)

type Score = M.Map Player Integer

type GameT = StateT Score IO


runGame :: GameT () -> IO ()
runGame game = evalStateT game score
  where score = M.empty

playHand :: (Player, Choice) -> (Player, Choice) -> GameT ()
playHand (p1, n1) (p2, n2) = do
  let total = n1 + n2
  let (winner, result) = if odd total then (p1, "odd") else (p2, "even")
  liftIO $ do
    putStrLn $ show p1 ++ ": " ++ show n1
    putStrLn $ show p2 ++ ": " ++ show n2
    putStrLn $ show total ++ " is " ++ result ++ " so " ++ show winner ++
               " wins this hand."
  modify $ recordWin winner

recordWin :: Player -> Score -> Score
recordWin p = M.alter inc p
  where inc Nothing  = Just 1
        inc (Just s) = Just (s + 1)

reportScore :: Player -> Player -> GameT ()
reportScore p1 p2 = do
  score <- get
  let s1 = M.findWithDefault 0 p1 score
  let s2 = M.findWithDefault 0 p2 score
  liftIO . putStrLn $ "\n" ++ show p1 ++ " (odds) " ++ show s1 ++ " x " ++
                      show s2 ++ " " ++ show p2 ++ " (evens)"

reportWinner :: Player -> Player -> GameT ()
reportWinner p1 p2 = do
  reportScore p1 p2
  score <- get
  let s1 = M.findWithDefault 0 p1 score
  let s2 = M.findWithDefault 0 p2 score
  liftIO . putStrLn $ case compare s1 s2 of
                        GT -> show p1 ++ " wins!"
                        LT -> show p2 ++ " wins!"
                        EQ -> "It's a draw!"

readInput :: Player -> IO (Input)
readInput p = do
  putStr $ "\n" ++ show p ++ ", pick a number or enter 'Q' or 'q' to quit: "
  input <- getLine
  case parseInput input of
    Just r  -> return r
    Nothing -> do putStrLn "Invalid input. Try again."
                  readInput p

parseInput :: String -> Maybe Input
parseInput s
  | null s              = Nothing
  | head s `elem` "qQ"  = Just Quit
  | all isDigit s       = Just $ Play (read s)
  | otherwise           = Nothing
