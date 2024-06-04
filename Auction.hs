module Auction where
import Cards(Board)
import Calls(Call(..), Level(..), Strain(..), Suit(..),Penalty(..))
import Player
import Play

import Control.Monad.Reader
import Control.Monad.State
import Data.List (find)
import Debug.Trace
import Text.Read hiding (get, lift)

data Auction = Auction {calls :: [(Direction,Call)], turn :: Direction} deriving Eq

instance Show Auction where
  show (Auction calls turn) = header ++ line where
    align string = " " ++ string ++ replicate (6 - length string) ' '
    header = 
      "|" ++ align (show West) ++
      "|" ++ align (show North) ++
      "|" ++ align (show East) ++
      "|" ++ align (show South) ++ "|"
    line = "\n---------------------------------\n"
    stringCalls = reverse (show turn):(map show calls)

mkAuction :: Direction -> Auction
mkAuction turn = Auction [] turn

result :: Auction -> Maybe Contract
result (Auction calls _) = case calls of
  (_, Pass):(_, Pass):(_, Pass):calls' | length calls' > 0
    -> Just $ resultHelper calls'
  _ -> Nothing

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
  auction <- get
  liftIO $ print auction
  case result auction of
    Just convention -> return convention
    _ -> do
      board <- lift ask
      case turn auction of
        South -> do
          -- liftIO $ print auction
          -- liftIO $ print "HAND"
          call <- liftIO $ getCallFromPlayer auction
          put $ Auction ((South, call):(calls auction)) (next South)
          runAuction
        _ -> do
          convention <- ask
          let call = convention board auction
          if elem call (availableCalls auction)
            then do
              put $ Auction ((turn auction, call):(calls auction)) (next $ turn auction)
              runAuction
            else error "The convention is written against the rules of the game!"
getCallFromPlayer :: Auction -> IO Call
getCallFromPlayer auction = do
  stringCall <- liftIO getLine
  case readMaybe stringCall of
    Just call | elem call (availableCalls auction) -> return call
    _ -> getCallFromPlayer auction

--- test
testAuction = Auction [(South, Pass)] (next South)
