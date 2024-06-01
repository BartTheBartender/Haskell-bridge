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
  let
    (_, call):_ = calls -- by properties of the auction
    penalty = case call of Pen pen -> Just pen; otherwise -> Nothing
    toLastBid =
      dropWhile(\(_, call) -> case call of (Bid _ _) -> False; _ -> True) calls

    (direction, (Bid level strain)):_ = toLastBid
    direction' =  fst $ firstDeclarer direction strain toLastBid
  in Just $ Contract level strain penalty direction'

result _ = Nothing

firstDeclarer direction' strain' calls' = last $ 
  filter(\(direction, call) -> (isPartner direction' direction)
  && case call of
    Bid _ strain -> strain == strain'
    _ -> False
  ) calls'


-- thisfunction assumes that the auction was constructed correctly
availableCalls :: Auction -> [Call]
availableCalls (Auction calls turn) =
  case noPasses of

    [] -> Pass:[Bid level strain 
      | level <- [minBound..maxBound], strain <- [minBound..maxBound]]

    (direction, call):_ -> if isPartner direction turn
      then 
        let
          (_, Bid level strain):_ = findLastBid noPasses
        in Pass:(higherBids level strain)
      else
        let (_, Bid level strain):_ = findLastBid noPasses
        in case call of
          Bid _ _ -> Pass:(Pen Double):(higherBids level strain)
          Pen Double -> Pass:(Pen Redouble):(higherBids level strain)
          Pen Redouble -> Pass:(higherBids level strain)
      
  where
  noPasses = dropWhile (\(_, call) -> case call of Pass -> True; _ -> False) calls
  higherBids level strain = 
    [ Bid level strain' | strain' <- [strain..maxBound], strain' /= strain] ++ 
    [Bid level' strain' | strain' <-[minBound..maxBound], level' <- [minBound..maxBound], level' > level]
  findLastBid calls' = dropWhile (\(_, call) -> case call of Pass -> True; _ -> False) calls'
--SANDBOX---------------

higherBids level strain = [(level, strain') | strain' <- [strain..maxBound], strain' /= strain] ++ [(level', strain') | strain' <-[minBound..maxBound], level' <- [minBound..maxBound], level' > level]
-- runAuction :: StateT Auction (ReaderT Convention (IO Call)) Play
