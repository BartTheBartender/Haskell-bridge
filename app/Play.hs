module Play(Play(..), Contract(..)) where
import Player(Direction(..))
import Cards(Board, mkBoard)
import Calls(Level(..), Strain(..), Penalty(..))

data Contract = Contract 
  {level :: Level, strain :: Strain, penalty :: Maybe Penalty, dealer :: Direction} | FourPasses deriving (Show, Eq)

-- data Play = Play {board :: Board, strain :: Strain, tricksNS :: Integer, lead :: Direction, declarer :: Direction}
data Play = Chuj


