module SAYC where
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
responseOn = undefined



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
  else if True then return Pass
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
  else if 6 <= hcp && hcp <= 10
    then if spades   >= 6 then return $ Bid Calls.Two (Trump Spade)
    else if hearts   >= 6 then return $ Bid Calls.Two (Trump Heart)
    else if diamonds >= 6 then return $ Bid Calls.Two (Trump Diamond)
    else if clubs    >= 6 then return $ Bid Calls.Two (Trump Club)
    else return Pass
  else return Pass

overcall :: Call -> Convention
overcall opening = do
  hand <- lift ask
  let hcp = highCardPoints hand

  if hcp >= 17 then return (Pen Double)
  else if hcp >= 8 && hcp <= 16 then do
    auction <- ask
    let longestSuit = longest $ map (\suit -> (suit, nofCards hand suit)) [minBound .. maxBound]
    if snd longestSuit < 5 || notElem (Bid Calls.One (Trump $ fst longestSuit)) (availableCalls auction) 
      then return Pass
      else return $ Bid Calls.One (Trump $ fst longestSuit)
  else return Pass

