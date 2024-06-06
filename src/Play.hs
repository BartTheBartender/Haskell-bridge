module Play where
import Player
import Cards
import Calls

import Control.Monad.Reader
import Control.Monad.State

data Contract = Contract 
  {level :: Level, strain :: Strain, penalty :: Maybe Penalty, dealer :: Direction} | FourPasses deriving (Show, Eq)

data Play = Play {
  contract :: Contract,
  lead :: Direction, 
  board :: Board,
  currentTrick :: [Card],
  nofTricks :: Int}

-- openingLead :: (Monad m) => Board -> Contract -> (Hand -> m Card) -> m Play
-- openingLead board contract mkOpeningLead = do
--   let lead = next $ dealer contract
--   let hand' = getHand board lead
--   card <- mkOpeningLead hand

