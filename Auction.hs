module Auction where
import Cards(Board)
import Calls(Call(..), Level(..), Strain(..), Suit(..))
import Player (Direction(..))
import Play(Play(..), Contract(..))

import Control.Monad.Reader
import Control.Monad.State


data Auction = Auction {calls :: [Call], turn :: Direction}

mkAuction :: Direction -> Auction
mkAuction turn = Auction [] turn

-- result :: Auction -> 
-- type Convention = ()
--
-- runAuction :: StateT Auction (ReaderT Convention (IO Call)) Play
