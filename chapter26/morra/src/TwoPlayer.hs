module TwoPlayer
    ( twoPlayerGame
    ) where


import Control.Monad.IO.Class         (liftIO)
import System.Console.ANSI            (clearScreen)
import Common


twoPlayerGame :: GameT ()
twoPlayerGame = do
  input1 <- readPlayerInput Player1
  case input1 of
    Quit    -> reportWinner Player1 Player2
    Play n1 -> do
      input2 <- readPlayerInput Player2
      case input2 of
        Quit   -> reportWinner Player1 Player2
        Play n2 -> do
          liftIO clearScreen
          playHand (Player1, n1) (Player2, n2)
          liftIO $ do
            putStr "\nPress any key to continue."
            getChar
          twoPlayerGame

readPlayerInput :: Player -> GameT (Input)
readPlayerInput player = do
  liftIO clearScreen
  reportScore Player1 Player2
  liftIO $ readInput player
