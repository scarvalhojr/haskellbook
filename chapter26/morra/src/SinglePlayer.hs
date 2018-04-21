module SinglePlayer
    ( singlePlayerGame
    ) where


import qualified Data.Map        as M (Map, empty, insert, lookup)
import Data.Maybe                     (isJust, fromJust)
import Control.Monad.IO.Class         (liftIO)
import System.Random                  (randomRIO)
import Common


type PastMoves = M.Map (Choice, Choice) Choice

type PrevChoice = Maybe Choice


singlePlayerGame :: GameT ()
singlePlayerGame = singlePlayerGame' prevs moves
  where prevs = (Nothing, Nothing)
        moves = M.empty

singlePlayerGame' :: (PrevChoice, PrevChoice) -> PastMoves -> GameT ()
singlePlayerGame' (p1, p2) moves = do
  reportScore Human Computer
  c <- liftIO $ computerChoice (p1, p2) moves
  input <- liftIO (readInput Human)
  case input of
    Quit   -> reportWinner Human Computer
    Play h -> do playHand (Human, h) (Computer, c)
                 let moves' = saveMoves (p1, p2) h moves
                 singlePlayerGame' (p2, Just h) moves'

computerChoice :: (PrevChoice, PrevChoice) -> PastMoves -> IO (Choice)
computerChoice prevs moves
  | isJust pred  = return (fromJust pred)
  | otherwise    = rand
  where pred = predictChoice prevs moves
        rand = randomRIO (0, 100)

predictChoice :: (PrevChoice, PrevChoice) -> PastMoves -> Maybe Choice
predictChoice (Just c1, Just c2) = M.lookup (c1, c2)
predictChoice _                  = const Nothing

saveMoves :: (PrevChoice, PrevChoice) -> Choice -> PastMoves -> PastMoves
saveMoves (Just c1, Just c2) c3 = M.insert (c1, c2) c3
saveMoves _                  _  = id
