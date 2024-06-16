module Conventions (
  biddingConvention,
  openingConvention,
  playingConvention,
  ) where
import Auction
import Cards
import Calls
import Player 
import Game

import Control.Monad.Reader
import Data.Maybe
import Data.Foldable (find)

import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)

biddingConvention :: BiddingConvention
biddingConvention = do
  auction <- ask
  hand <- lift $ ask
  -- opening
  if isOpening auction then opening
  -- overcall
  else if isJust $ overcallAfter auction then let Just opening = overcallAfter auction
    in overcall opening
  -- response
  else if isJust $ responseOn auction then let Just opening = responseOn auction
    in response opening
  else return Pass

openingConvention :: OpeningConvention
openingConvention = do
  contract :: Contract <- ask
  case strain contract of
    Trump suit -> openingTrump suit
    NoTrump -> openingNoTrump

playingConvention :: PlayingConvention
playingConvention = do
  myHand <- ask
  myDirection <- lift ask
  currTrick <- lift $ lift $ lift ask
  contract <- lift $ lift $ lift $ lift ask
  let strain' = strain contract
  case nofHand currTrick of
    1 -> 
      if dummy contract == next myDirection then leadViaDummy
      else if dummy contract == prev myDirection then leadToDummy
      else leadDealer
    2 -> return $ garbage currTrick myHand
    3 -> return $ strongestWinning strain' currTrick myHand
    4 -> return $ weakestWinning strain' currTrick myHand

--------------------------------------------------------------------------------
-- general utility functions

highCardPoints :: Hand -> Int
highCardPoints (Hand cards) = foldl (\acc card -> case figure card of
    Ace   -> acc + 4
    King  -> acc + 3
    Queen -> acc + 2
    Jack  -> acc + 1
    _ -> acc) 0 cards

nofCards :: Hand -> Suit -> Int
nofCards cards suit' = length $ getSuit cards suit'

suitLengths :: Hand -> [(Suit,Int)]
suitLengths hand = 
  map (\suit' -> (suit', nofCards hand suit')) [minBound..maxBound]

longestSuit :: Hand -> (Suit, Int)
longestSuit hand = foldl1(\x y -> longer x y) $ suitLengths hand where
  longer x y = if snd x == snd y
    then 
      if fst x > fst y then x else y
    else 
      if snd x > snd y then x else y

distribution hand = map (nofCards hand) [minBound..maxBound]
-- A hand is balanced if it has no void and no singleton, doubletons at most two
isBalanced :: Hand -> Bool
isBalanced hand = all (>2) (distribution hand) && 
  length (filter (==2) (distribution hand)) <= 2
--------------------------------------------------------------------------------
-- utility functions for biddingConvention

isOpening :: Auction -> Bool
isOpening = all ((== Pass) . snd) . calls

overcallAfter :: Auction -> Maybe Call
overcallAfter auction 
  | nofCalls auction >= 3 && (isOpening.stepBack.stepBack.stepBack $ auction)
    = case lastCall.stepBack.stepBack $ auction of
      Bid level strain -> Just $ Bid level strain
      _ -> Nothing
  | nofCalls auction >= 1 && (isOpening.stepBack $ auction)
    = case lastCall auction of
    Bid level strain -> Just $ Bid level strain
    _ -> Nothing
  | otherwise = Nothing

responseOn :: Auction -> Maybe Call
responseOn auction
  | nofCalls auction >= 2 && (isOpening.stepBack.stepBack $ auction)
    = case lastCall.stepBack $ auction of
    Bid level strain -> Just $ Bid level strain
    _ -> Nothing
  | otherwise = Nothing


lastNoPass :: Auction -> Maybe Call
lastNoPass (Auction [] _) = Nothing
lastNoPass auction@(Auction  ((_, call):_) _) = 
  if call == Pass then lastNoPass $ stepBack auction else Just call

opening :: BiddingConvention
opening = do
  hand <- lift $ ask
  let hcp = highCardPoints hand
      spades   = nofCards hand Spade
      hearts   = nofCards hand Heart
      diamonds = nofCards hand Diamond
      clubs    = nofCards hand Club

  -- Notrump openings
  if 15 <= hcp && hcp <= 17 && isBalanced hand 
    then return $ Bid Calls.One NoTrump 
  else if 20 <= hcp && hcp <= 21 && isBalanced hand 
    then return $ Bid Calls.Two NoTrump 
  else if 25 <= hcp && hcp <= 27 && isBalanced hand 
    then return $ Bid Calls.Three NoTrump
  -- One color openings
  else if 12 <= hcp && hcp <= 21 then
    -- One major
    if spades >= 5 || hearts >= 5 then 
      if spades >= hearts 
        then return $ Bid Calls.One (Trump Spade)
        else return $ Bid Calls.One (Trump Heart)
    -- One minor
    else 
      if diamonds >= clubs
        then return $ Bid Calls.One (Trump Diamond)
        else return $ Bid Calls.One (Trump Club)
  -- Preemptive
  else if 5 <= hcp && hcp <= 11
    then if spades   >= 7 then return $ Bid Calls.Three (Trump Spade)
    else if hearts   >= 7 then return $ Bid Calls.Three (Trump Heart)
    else if diamonds >= 7 then return $ Bid Calls.Three (Trump Diamond)
    else if clubs    >= 7 then return $ Bid Calls.Three (Trump Club)

    else return Pass
  -- plain old strong 2
  else if 22 <= hcp
    then if spades   >= 5 then return $ Bid Calls.Two (Trump Spade)
    else if hearts   >= 5 then return $ Bid Calls.Two (Trump Heart)
    else if diamonds >= 5 then return $ Bid Calls.Two (Trump Diamond)
    else return $ Bid Calls.Two (Trump Club)
  else return Pass

-- simplified SAYC
overcall :: Call -> BiddingConvention
overcall opening = do
  hand <- lift ask
  let hcp = highCardPoints hand

  if hcp >= 17 then return (Pen Double)
  else if hcp >= 8 && hcp <= 16 then do
    auction <- ask
    let longest = longestSuit hand
    if snd longest < 5 || notElem (Bid Calls.One (Trump $ fst longest)) (availableCalls auction) 
      then return Pass
      else return $ Bid Calls.One (Trump $ fst longest)
  else return Pass

-- really simplified SAYC - assumes bots sign-off the auction
response :: Call -> BiddingConvention
response (Bid Calls.One NoTrump) = do
  hand <- lift $ ask
  let hcp = highCardPoints hand
  if 10 <= hcp then do
    auction <- ask
    let _3NT = Bid Calls.Two NoTrump
    if elem _3NT (availableCalls auction) then return _3NT else return Pass
  else if 8 <= hcp && hcp <= 9 then do
    auction <- ask
    let _2NT = Bid Calls.Two NoTrump
    if elem _2NT (availableCalls auction) then return _2NT else return Pass
  else return Pass

response (Bid Calls.Two NoTrump) = do
  hand <- lift $ ask
  let hcp = highCardPoints hand
  if 4 <= hcp then do
    auction <- ask
    let _3NT = Bid Calls.Two NoTrump
    if elem _3NT (availableCalls auction) then return _3NT else return Pass
  else return Pass

response (Bid Calls.One (Trump suit)) = do
  hand <- lift $ ask
  let hcp = highCardPoints hand
  if hcp <= 6 then return Pass
  else 
    let cardsInSuit = nofCards hand suit
        fit = if isMajor suit then cardsInSuit >= 3 else cardsInSuit >= 5
    in if fit 
       then 
         return $ if hcp <= 9 then Bid Calls.Two (Trump suit)
                  else if hcp <= 12 then Bid Calls.Three (Trump suit)
                  else Bid Calls.Four (Trump suit)
       else return Pass

-- this is a disgrace to sayc, but bots are not clever enough
response (Bid Calls.Two (Trump Club)) = do
  hand <- lift $ ask
  let hcp = highCardPoints hand
  if hcp <= 2 then return $ Pass
  else 
    let fit = nofCards hand Club >= 5
    in if fit then return $ Bid Calls.Five (Trump Club)
    else return $ Bid Calls.Three NoTrump

response (Bid Calls.Two (Trump suit)) = do
  hand <- lift $ ask
  let hcp = highCardPoints hand
  if hcp <= 2 then return $ Pass
  else 
    let cardsInSuit = nofCards hand suit
        fit = if isMajor suit then cardsInSuit >= 3 else cardsInSuit >= 5
    in if fit 
      then if isMajor suit then return $ Bid Calls.Four (Trump suit)
      else return $ Bid Calls.Five (Trump suit)
    else return $ Bid Calls.Three NoTrump

response _ = return Pass

--------------------------------------------------------------------------------
-- utility functions for openingConvention


openingNoTrump :: OpeningConvention
openingNoTrump = do
  hand :: Hand <- lift ask
  let longest = (reverse $ getSuit hand (fst $ longestSuit hand))
  return $ trace (show longest) (longest !! 3)
  -- safety: length hand = 13 = s + h + d + c. By pigeonhole one of them is >= 4

openingTrump :: Suit -> OpeningConvention
openingTrump trump = do
  hand <- lift ask
  let singletonSuit = fmap fst $ find (\(_, length') -> length' == 1) $ suitLengths hand
  case singletonSuit of
    Just suit' | suit' /= trump -> 
      let [card] = getSuit hand suit' in return card
    _ -> openingNoTrump

--------------------------------------------------------------------------------
-- playing utilities

nofHand :: Trick -> Int
nofHand trick | length trick <= 3 = length trick + 1
              | otherwise = error $ "The trick " ++ show trick ++ " is done!"

winningCards :: Strain -> Trick -> Hand -> [Card]
winningCards NoTrump trick hand =
  filter (\myCard -> (all (\trickCard -> trickCard < myCard) trick)) 
  (availableCards trick hand)

winningCards (Trump trump) trick hand =
  if null trumpsInTrick
    then winningCards NoTrump trick hand
    else 
      filter(
        \myCard -> suit myCard == trump &&
          (all (\trickCard -> trickCard < myCard) trumpsInTrick)
      ) (availableCards trick hand)
  where
    trumpsInTrick :: [Card] = filter (\trickCard -> suit trickCard == trump) trick

garbage :: Trick -> Hand -> Card -- safety: hand is nonempty (playGame)
garbage trick hand = minimum $ availableCards trick hand

-- if there is no such card, then play garbage
strongestWinning :: Strain -> Trick -> Hand -> Card
strongestWinning strain trick hand = 
  if null winning then garbage trick hand else maximum winning
  where winning = winningCards strain trick hand

weakestWinning :: Strain -> Trick -> Hand -> Card
weakestWinning strain trick hand = 
  if null winning then garbage trick hand else maximum winning
  where winning = winningCards strain trick hand

leadToDummy :: PlayingConvention
leadToDummy = do

  myHand <- ask
  dummy <- lift $ lift ask
  strain' <- lift $ lift $ lift $ lift $ asks $ strain

  let smallestSuit = fst $ foldl1 (\(suit, len) (suit', len') -> if len > len' then (suit', len') else (suit, len)) $ suitLengths dummy
  
  if not $ null $ getSuit myHand smallestSuit
    then return $ maximum $ getSuit myHand smallestSuit
    else return $ maximum $ getSuit myHand (fst $ longestSuit myHand)



leadViaDummy :: PlayingConvention
leadViaDummy = do
  myHand <- ask
  dummy <- lift $ lift ask
  strain' <- lift $ lift $ lift $ lift $ asks $ strain

  let highestCard = strongestWinning strain' [] dummy
  
  if not $ null $ getSuit myHand (suit highestCard)
    then return $ minimum $ getSuit myHand (suit highestCard)
    else return $ maximum $ getSuit myHand (fst $ longestSuit myHand)

leadDealer :: PlayingConvention
leadDealer = do
  myHand <- ask
  otherHand <- lift $ lift ask
  strain' <- lift $ lift $ lift $ lift $ asks $ strain

  case strain' of
    Trump trump | not $ null $ getSuit myHand trump 
      -> return $ head $ getSuit myHand trump
    _ -> do
      let highestCard = strongestWinning NoTrump [] otherHand
      if (not $ null $ getSuit myHand (suit highestCard)) && 
         (figure highestCard == Ace ||
          figure highestCard == King ||
          figure highestCard == Queen)
        then return $ minimum $ getSuit myHand (suit highestCard)
        else return $ maximum $ getSuit myHand (fst $ longestSuit myHand)
    
