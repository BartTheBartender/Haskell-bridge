module Auction where
import Cards(Board)
import Calls(Call(..), Level(..), Strain(..), Suit(..),Penalty(..))
import Player (Direction(..), prev, next, isPartner)
import Play(Play(..), Contract(..))

import Control.Monad.Reader
import Control.Monad.State
import Data.List (find)
import Debug.Trace


data Auction = Auction {calls :: [(Direction,Call)], turn :: Direction} deriving Eq

instance Show Auction where
  show (Auction calls turn) = header ++ showCalls calls ++ showTurn turn
    where
    line = "\n---------------------------------\n"
    header =  "|" ++ align (show West) ++ "|" ++ align (show North) ++ "|" ++ align (show East) ++ "|" ++ align (show South)

    showTurn South = "|" ++ align "#" ++ "|"
    showTurn East  = "|" ++ align "#" ++ "|" ++ align "" ++ "|"
    showTurn North = "|" ++ align "#" ++ "|" ++ align "" ++ "|" ++ align "" ++ "|"
    showTurn West  = "|" ++ line ++ "|" ++ align "#" ++ "|" ++ align "" ++ "|" ++ align "" ++ "|" ++ align "" ++ "|"

    showCalls [] = ""
    showCalls ((West, call):xs) = showCalls xs ++ "|" ++ line ++ "|" ++ align(show call)
    showCalls ((_, call):xs) = showCalls xs ++ "|" ++ align(show call)

    align s | length s <= 5 = " " ++ s ++ take (6 - length s) [' ',' '..]

-- auction0 = Auction [(South, Pass), (East, Pass), (North, Pass),(West, (Bid One NoTrump)), (South, Pass), (East, Pass), (North, Pass), (West, Pass)] North
--
-- auction1 = Auction [(South, Pass), (East, Pass), (North, Pass),(West, Pass), (South, Pass), (East, Pass), (North, Pass), (West, Pass)] North

auction0 = Auction [(South, Pass),(East, Pass),(North, Pass),(West, Pen Redouble),(South, Pen Double),(East, Bid Three (Trump Spade)), (North, Bid Three (Trump Club)), (West, Bid Two (Trump Spade)), (South, Pass), (East, Pen Double), (North, Bid One (Trump Heart)), (West, Pass)] West

mkAuction :: Direction -> Auction
mkAuction turn = Auction [] turn

result :: Auction -> Maybe Contract
result (Auction ((_, Pass):(_, Pass):(_, Pass):calls) _) = Just $ resultHelper calls
result _ = Nothing

resultHelper [(_, Pass)] = FourPasses
resultHelper ((_, Pass):_) = error "The auction was constructed incorrectily!"
resultHelper calls = 
  let (_, lastCall):_ = calls
      penalty = case lastCall of Pen pen -> Just pen; _ -> Nothing
      toLastBid = 
        dropWhile (\(_, call) -> case call of (Bid _ _) -> False; _ -> True) calls
      (lastDirection, (Bid level strain)):_ = toLastBid
      direction = fst $ last $ 
        filter(\(_, call) -> case call of Bid _ strain -> True; _ -> False)
        (filter((isPartner lastDirection).fst) toLastBid)

  in Contract level strain penalty direction

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

higherBids level strain = [(level, strain') | strain' <- [strain..maxBound], strain' /= strain] ++ [(level', strain') | strain' <-[minBound..maxBound], level' <- [minBound..maxBound], level' > level]

type Convention = Board -> Auction -> Call
badConvention = \_ _ -> Pass

runAuction :: ReaderT Convention (ReaderT Board (StateT Auction IO)) Contract
runAuction = do
  -- convention <- ask
  -- board <- lift ask
  auction <- lift $ lift get
  case result auction of
    Just contract -> do
      liftIO $ putStrLn "Auction finished"
      return contract
    Nothing -> do
      board <- lift ask
      liftIO $ print "a"
      case turn auction of
        South -> undefined
        _ -> undefined
