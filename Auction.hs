module Auction where
import Cards(Board)
import Calls(Call(..), Level(..), Strain(..), Suit(..),Penalty(..))
import Player (Direction(..), prev, next, isPartner)
import Play(Play(..), Contract(..))

import Control.Monad.Reader
import Control.Monad.State
import Data.List (find)
import Debug.Trace


data Auction = Auction {calls :: [(Direction,Call)], turn :: Direction} deriving (Show, Eq)

auction0 = Auction [(South, Pass), (East, Pass), (North, Pass),(West, (Bid One NoTrump)), (South, Pass), (East, Pass), (North, Pass), (West, Pass)] North

auction1 = Auction [(South, Pass), (East, Pass), (North, Pass),(West, Pass), (South, Pass), (East, Pass), (North, Pass), (West, Pass)] North

mkAuction :: Direction -> Auction
mkAuction turn = Auction [] turn

-- private function. Assumes the auction is constructed correctly.
result :: Auction -> Maybe Contract
result (Auction [(_, Pass), (_, Pass), (_, Pass), (_, Pass)] _) = Just FourPasses
result (Auction ((_, Pass):(_, Pass):(_, Pass):calls) _) =
  Just $ Contract level strain penalty direction' where
  (_, call):_ = calls
  penalty = case call of Pen pen -> Just pen; _ -> Nothing
  toLastBidxs = toLastBid calls
  (direction, (Bid level strain)):_ = toLastBidxs
  (direction', _) = firstDeclarer direction strain toLastBidxs
result _ = Nothing
firstDeclarer direction' strain' calls' = last $ 
  filter(\(direction, call) -> (isPartner direction' direction)
  && case call of
    Bid _ strain -> strain == strain'
    _ -> False
  ) calls'
toLastBid calls' = dropWhile(\(_, call) -> case call of (Bid _ _) -> False; _ -> True) calls'


-- this function assumes that the auction was constructed correctly
availableCalls :: Auction -> [Call]
availableCalls (Auction calls turn) =
  case noPasses of
  [] -> Pass:[Bid level strain | level <- [minBound..maxBound], strain <- [minBound..maxBound]]
  (direction, strain):_ -> if isPartner direction turn
    then undefined
    else undefined
  where
  noPasses = dropWhile (\(_, call) -> case call of Pass -> True; _ -> False) calls

--SANDBOX---------------

-- runAuction :: StateT Auction (ReaderT Convention (IO Call)) Play
