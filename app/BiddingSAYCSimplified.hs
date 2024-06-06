module BiddingSAYCSimplified where
import Auction
import Cards
import Calls
import Player

import Control.Monad.Reader
import Data.Maybe
import Debug.Trace

highCardPoints :: Hand -> Int
highCardPoints (Hand cards) = foldl (\acc card -> case figure card of
    Ace   -> acc + 4
    King  -> acc + 3
    Queen -> acc + 2
    Jack  -> acc + 1
    _ -> acc) 0 cards

nofCards :: Hand -> Suit -> Int
nofCards (Hand cards) suit' = length $ filter (\card -> suit card == suit') cards

longestSuit hand = longest $ map (\suit -> (suit, nofCards hand suit)) [minBound .. maxBound] where
  longest (x:[]) = x
  longest (x:y:xs) = if snd x == snd y 
    then if fst x > fst y then longest (x:xs) else longest (y:xs)
    else if snd x > snd y then longest (x:xs)
    else longest (y:xs)

-- A hand is balanced if it has no void and no singleton, doubletons at most two
isBalanced :: Hand -> Bool
isBalanced hand = all (>2) distribution && length (filter (==2) distribution) <= 2
  where distribution = map (nofCards hand) [minBound..maxBound]

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



simpleConvention :: Convention
simpleConvention = do
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

lastNoPass :: Auction -> Maybe Call
lastNoPass (Auction [] _) = Nothing
lastNoPass auction@(Auction  ((_, call):_) _) = 
  if call == Pass then lastNoPass $ stepBack auction else Just call

opening :: Convention
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
overcall :: Call -> Convention
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
response :: Call -> Convention
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
