module Cards(
  Suit(..),
  isMajor, isMinor,
  Figure(..),
  Card(..),
  Hand(..),
  getSuit,
  Board,
  getHand,
  mkBoard,
  playCard,
  isEmpty
  ) where

import Player

import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')
import Data.Array
import Data.List

import Debug.Trace

data Suit = Club | Diamond | Heart | Spade deriving (Eq, Ord, Bounded, Enum)

instance Show Suit where
  show Spade = ['\x2660']
  show Heart = ['\x2665']
  show Diamond = ['\x2666']
  show Club = ['\x2663']

isMajor :: Suit -> Bool
isMajor suit = suit == Spade || suit == Heart

isMinor :: Suit -> Bool
isMinor = not . isMajor

data Figure = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
              Jack | Queen | King | Ace deriving (Eq, Ord, Bounded, Enum)

instance Show Figure where
  show Ace   = "A"
  show King  = "K"
  show Queen = "Q"
  show Jack  = "J"
  show Ten   = show 10
  show Nine  = show 9
  show Eight = show 8
  show Seven = show 7
  show Six   = show 6
  show Five  = show 5
  show Four  = show 4
  show Three = show 3
  show Two   = show 2

data Card = Card { figure :: Figure, suit :: Suit } deriving (Eq, Bounded)

instance Ord Card where
  compare (Card figure suit) (Card figure' suit') = 
    if figure == figure' then compare suit suit' else compare figure figure'

instance Show Card where
  show (Card figure suit) 
    | suit == Spade || suit == Club = 
        "\ESC[38;5;16m" ++ show figure ++ show suit ++ "\ESC[0m"
    | suit == Heart || suit == Diamond = 
        "\ESC[31;5;16m" ++ show figure ++ show suit ++ "\ESC[0m"

newtype Hand = Hand [Card]

instance Show Hand where
  show (Hand xs) = intercalate " " $ map show xs

getSuit :: Hand -> Suit -> [Card]
getSuit (Hand hand) suit' = filter (\card -> suit card == suit') hand

getLength :: Hand -> Int
getLength (Hand hand) = length hand

data Board = Board (Array Direction Hand)

getHand :: Board -> Direction -> Hand
getHand (Board arr) direction = arr ! direction

isEmpty :: Board -> Bool
isEmpty board = all ((== 0).getLength) $ map (getHand board) [minBound..maxBound]

instance Show Board where
  show board = unlines $ map (\dir ->
    show dir ++ ": " ++ show (getHand board dir)
    ) [minBound..maxBound]

deck :: [Card]
deck = [Card figure suit | suit <- [minBound..maxBound], figure <- [minBound..maxBound]]

mkBoard :: IO Board
mkBoard = do
  gen <- newStdGen
  let helper = zip [0..] $ shuffle' deck 52 gen
  return $ Board $ array
    (minBound, maxBound) $
    map (\dir -> (dir, 
      Hand $ sortBy bySuit $ map snd $ filter (\(i, _) -> i `mod` 4 == fromEnum dir) helper
    )) [minBound..maxBound]
    where
      bySuit card card' = if suit card == suit card' 
        then compare (figure card) (figure card')
        else compare (suit card) (suit card')


playCard :: Board -> Direction -> Card -> Board
playCard board direction card = 
  if card `elem` cards 
  then 
    Board $ array (minBound, maxBound) $
      map (\(dir, Hand hand) -> (dir, Hand $ filter (/= card) hand)) $
      map (\dir -> (dir, getHand board dir)) [minBound..maxBound]
  else 
    error $ "The card " ++ show card ++ " doesn't belong to " ++ show direction
  where
    Hand cards = getHand board direction


