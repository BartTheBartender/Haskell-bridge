module Conventions (
  biddingConvention,
  openingConvention,
  dealingConvention,
  defendingConvention,
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

dealingConvention :: DealingConvention
dealingConvention = do
  contract :: Contract <- lift $ lift ask
  case strain contract of
    Trump suit -> dealingTrump suit
    NoTrump -> dealingNoTrump


defendingConvention :: DefendingConvention
defendingConvention = undefined


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

-- A hand is balanced if it has no void and no singleton, doubletons at most two
isBalanced :: Hand -> Bool
isBalanced hand = all (>2) distribution && length (filter (==2) distribution) <= 2
  where distribution = map (nofCards hand) [minBound..maxBound]
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
  let suit'= fst $ longestSuit hand
  let _ = unsafePerformIO $ print hand
  return $ (reverse $ getSuit hand suit') !! 4
  -- safety: length hand = 13 = s + h + d + c. By pigeonhole one of them is >= 4

openingTrump :: Suit -> OpeningConvention
openingTrump trump = do
  hand <- lift ask
  let singletonSuit = fmap fst $ find (\(_, length') -> length' == 1) $ suitLengths hand
  case singletonSuit of
    Just suit' | suit' /= trump -> 
      let [card] = getSuit hand suit' in return card
    _ -> openingNoTrump

-- playing utilities

nofHand :: Trick -> Int
nofHand trick | length trick <= 3 = length trick + 1

availableTrumps :: Suit -> Trick -> Hand -> [Card]
availableTrumps trump trick hand = 
  filter (\card -> suit card == trump) (availableCards trick hand)

lowestBest :: Strain -> Trick -> Hand -> Maybe Card
lowestBest NoTrump trick hand = 
  find (\myCard -> (all (\trickCard -> trickCard < myCard) trick)) 
  (availableCards trick hand)

lowestBest (Trump trump) trick hand = 
  case lowestBest NoTrump trick hand of
    Just card -> Just card
    Nothing -> find (const True) (getSuit hand trump)

dealingNoTrump :: DealingConvention
dealingNoTrump = do
  (tricks, otherHand) <- lift $ ask
  myHand <- ask
  let currTrick:_ = tricks
  let avCards = availableCards currTrick myHand

  case nofHand currTrick of
    1 ->
      return $ head $ getSuit myHand (fst $ longestSuit myHand)
    2 ->
      return $ minimum $ avCards
    3 ->
      return $ maximum $ avCards
    4 -> case lowestBest NoTrump currTrick myHand of
          Just card -> return card
          Nothing -> return $ head avCards

dealingTrump :: Suit -> DealingConvention
dealingTrump trump = do
  myHand <- ask
  (tricks, otherHand) <- lift $ ask
  let currTrick:_ = tricks
  let avCards = availableCards currTrick myHand

  let nofOpponentsTrumps :: Int = (\x -> 13-x).length $
        (getSuit myHand trump) ++ (getSuit otherHand trump) ++
        filter (\card -> suit card == trump) (concat tricks)

  let avTrumps = availableTrumps trump currTrick myHand
  let trumpsInTrick = reverse $ filter (\trickCard -> suit trickCard == trump) currTrick
  if nofOpponentsTrumps /= 0
    then case nofHand currTrick of -- draw trumps
      1 -> if null avTrumps
            then return $ head $ getSuit myHand (fst $ longestSuit myHand)
            else return $ head avTrumps
      2 -> if null avTrumps
              then return $ head avCards
              else return $ head avTrumps
      3 -> if null avTrumps
              then return $ last avCards
              else return $ last avTrumps
      4 -> case lowestBest (Trump trump) currTrick myHand of
              Just card -> return card
              Nothing -> 
                case lowestBest NoTrump currTrick myHand of
                  Just card -> return card
                  Nothing -> return $ head avCards


    else case nofHand currTrick of -- take advantage of trumps
          1 -> return $ head $ getSuit myHand (fst $ longestSuit myHand)
          2 -> return $ head avCards -- snd low
          3 -> if null avTrumps -- third high
                then
                  return $ last avTrumps
                else
                  return $ last avCards

          4 -> if null trumpsInTrick -- 4th has to think again
                then case lowestBest (Trump trump) currTrick myHand of
                  Just card -> return card
                  Nothing -> if null avTrumps 
                    then return $ head avCards
                    else return $ head avTrumps

                else
                  if null avTrumps
                    then return $ head avCards
                    else return $ head avTrumps
