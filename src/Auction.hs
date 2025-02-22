module Auction where
import Cards
import Calls
import Player
import Game (Contract(..))

import Control.Monad.Reader
import Control.Monad.State
import Data.List (intercalate, find)
import Debug.Trace
import Text.Read hiding (get, lift)
import System.Process

data Auction = Auction {calls :: [(Direction,Call)], turn :: Direction} deriving Eq

instance Show Auction where
  show (Auction calls turn) = intercalate line $
    map (\x -> "|" ++ (intercalate "|" x) ++ "|") (chunks cells) where
    align string = " " ++ string ++ replicate (6 - length string) ' '
    line = "\n---------------------------------\n"
    cells = map align $
            map show [West, North, East, South] ++
            replicate spacesFront " " ++
            reverse (map (show.snd) calls) ++
            ["#"] ++ 
            replicate spacesBack " " where
      spacesFront = 
        if length calls == 0 then fromEnum turn else fromEnum $ fst (last calls)
      spacesBack = 3 - fromEnum turn
    chunks xs = if length xs == 0 then [] else (take 4 xs):(chunks $ drop 4 xs)

stepBack :: Auction -> Auction
stepBack (Auction (_:calls) turn) = Auction calls (pred turn)

mkAuction :: Direction -> Auction
mkAuction turn = Auction [] turn

nofCalls :: Auction -> Int
nofCalls = length.calls

lastCall :: Auction -> Call
lastCall (Auction ((_, call):_) _) = call
lastCall auction = error $ "The auction didn't start\n" ++ show auction

addCall :: Auction -> Call -> Auction
addCall (Auction calls turn) call = Auction ((turn,call):calls) (next turn)

result :: Auction -> Maybe Contract
result (Auction calls _) = case calls of
  (_, Pass):(_, Pass):(_, Pass):calls' | length calls' > 0
    -> Just $ resultHelper calls'
  _ -> Nothing

resultHelper [(_, Pass)] = FourPasses
resultHelper ((_, Pass):_) = error "The auction was constructed incorrectly!"
resultHelper calls = case calls of
  (direction, Bid level strain):rest
    -> Contract level strain Nothing (findFirst rest direction strain)

  (_, Pen Double):(direction, Bid level strain):rest
    -> Contract level strain (Just Double) (findFirst rest direction strain)

  (_, Pen Double):(_, Pass):(_, Pass):(direction, Bid level strain):rest
    -> Contract level strain (Just Double) (findFirst rest direction strain)

  (_, Pen Redouble):(_, Pen Double):(direction, Bid level strain):rest
    -> Contract level strain (Just Redouble) (findFirst rest direction strain)

  (_, Pen Redouble):(_, Pass):(_, Pass):(_, Pen Double):(direction, Bid level strain):rest
    -> Contract level strain (Just Redouble) (findFirst rest direction strain)

  (_, Pen Redouble):(_, Pen Double):(_, Pass):(_, Pass):(direction, Bid level strain):rest
    -> Contract level strain (Just Redouble) (findFirst rest direction strain)

  (_, Pen Redouble):(_, Pass):(_, Pass):(_, Pen Double):(_, Pass):(_, Pass):(direction, Bid level strain):rest
    -> Contract level strain (Just Redouble) (findFirst rest direction strain)

  _ -> error "The auction was constructed incorrectly!"
  where
    findFirst :: [(Direction, Call)] -> Direction -> Strain -> Direction
    findFirst rest direction strain =
      let 
        pairCalls = filter (\(direction', _) -> isPartner direction direction) rest
        onlyBidsInStrain = filter(\(_, call) -> case call of
          Bid _ strain' | strain == strain' -> True
          otherwise -> False) pairCalls
      in if (not.null) onlyBidsInStrain
        then fst $ last onlyBidsInStrain
        else direction

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

type BiddingConvention = ReaderT Auction (Reader Hand) Call

runAuction :: BiddingConvention -> Board -> StateT Auction IO Contract
runAuction convention board = do
  auction <- get
  case result auction of
    Just contract -> do
      lift $ system "clear"
      lift $ print auction
      return contract
    Nothing -> do
      
      case turn auction of
        South -> do
          lift $ system "clear"
          lift $ print auction
          lift $ print (getHand board South)
          call <- lift $ getCallFromPlayer auction
          put $ addCall auction call
          runAuction convention board
        _ -> do
          let hand = getHand board (turn auction)
              call = runReader (runReaderT convention auction) hand
          if elem call (availableCalls auction)
            then do
              put $ addCall auction call
              runAuction convention board
            else
              error $ 
                "Convention is incorrect - tried to make call " ++ show call ++
                " in auction " ++ show auction ++ "."

getCallFromPlayer :: Auction -> IO Call
getCallFromPlayer auction = do
  stringCall <- getLine
  case readMaybe stringCall of
    Just call | elem call (availableCalls auction) -> return call
              | otherwise -> do
                    mapM_ putStr ["The call: ", (show call), " is not available.\n"]
                    getCallFromPlayer auction
    _ -> do
      putStrLn "Usage: 'Pass' - Pass, 'x' - Double, 'xx' - Redouble, example bid: 1NT"
      getCallFromPlayer auction

