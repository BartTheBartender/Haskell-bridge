module Play(Play(..), Contract(..)) where
import Player(Direction(..))
import Cards(Board, mkBoard)
import Calls(Strain(..), Penalty(..))

data Contract = Contract {strain :: Strain, penalty :: Penalty, dealer :: Direction}

-- data Play = Play {board :: Board, strain :: Strain, tricksNS :: Integer, lead :: Direction, declarer :: Direction}
data Play = Chuj


