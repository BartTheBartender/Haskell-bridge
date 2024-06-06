module Cards where
import Player

import System.Random.Shuffle (shuffleM)
import Control.Monad.Random
import Data.Array
import Data.List

data Suit = Club | Diamond | Heart | Spade deriving (Eq, Ord, Bounded, Enum)

instance Show Suit where
  show Spade = ['\x2660']
  show Heart = ['\x2665']
  show Diamond = ['\x2666']
  show Club = ['\x2663']

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

data Card = Card {figure :: Figure, suit :: Suit} deriving (Eq, Bounded)

instance Ord Card where
  compare (Card figure suit) (Card figure' suit') = if suit == suit' then compare figure figure' else compare suit suit'

instance Show Card where
  show (Card figure suit) 
    | suit == Spade || suit == Club = 
        "\ESC[38;5;16m" ++ show figure ++ show suit ++ "\ESC[0m"  -- Dark black-like color
    | suit == Heart || suit == Diamond = 
        "\ESC[38;5;1m" ++ show figure ++ show suit ++ "\ESC[0m"  -- Bright red color

data Hand = Hand [Card]

instance Show Hand where
  show (Hand xs) = intercalate " " $ map show xs

data Board = Board (Array Direction Hand)

getHand :: Board -> Direction -> Hand
getHand (Board arr) direction = arr ! direction

instance Show Board where
  show (Board arr) = 
    show South ++ ": " ++ show (arr ! South) ++ "\n" ++
    show West ++ " : " ++ show (arr ! West ) ++ "\n" ++ 
    show North ++ ": " ++ show (arr ! North) ++ "\n" ++
    show East ++ " : " ++ show (arr ! East )



deck :: [Card]
deck = [Card figure suit | suit <-[minBound..maxBound], figure <-[minBound..maxBound]]

mkBoard :: (RandomGen g) => Rand g Board
mkBoard = do
  shuffled <- shuffleM deck
  let helper = zip [0,1..] shuffled
  let south = Hand $ sort $ map(snd) $ filter(\x -> (fst x) `mod` 4 == 0) helper
  let west  = Hand $ sort $ map(snd) $ filter(\x -> (fst x) `mod` 4 == 1) helper
  let north = Hand $ sort $ map(snd) $ filter(\x -> (fst x) `mod` 4 == 2) helper
  let east  = Hand $ sort $ map(snd) $ filter(\x -> (fst x) `mod` 4 == 3) helper
  return (Board $ array(minBound, maxBound) [(South, south), (West, west), (North, north), (East, east)])

isMajor :: Suit -> Bool
isMajor suit = suit == Spade || suit == Heart

isMinor :: Suit -> Bool
isMinor = not.isMajor

