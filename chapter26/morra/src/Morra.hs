module Morra
    ( playMorra
    ) where


import System.IO     (hSetBuffering, stdout, BufferMode(..))
import Common        (runGame)
import SinglePlayer  (singlePlayerGame)
import TwoPlayer     (twoPlayerGame)


data GameMode = SinglePlayer | TwoPlayers


playMorra :: IO ()
playMorra = do
  hSetBuffering stdout NoBuffering
  putStrLn "\nThe Morra Game"
  mode <- readGameMode
  runGame $ case mode of
              SinglePlayer -> singlePlayerGame
              TwoPlayers   -> twoPlayerGame

readGameMode :: IO (GameMode)
readGameMode = do
  putStr $ "\nChoose number of players (1 or 2): "
  input <- getLine
  case input of
    ['1'] -> return SinglePlayer
    ['2'] -> return TwoPlayers
    _     -> do putStrLn "Invalid input. Try again."
                readGameMode
