module Cards (Suit(..), Figure, Card, suit, figure, Board, mkBoard) where
import Player(Direction(..), next)

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
  show (Card figure suit) = show figure ++ show suit

data Board = Board (Array Direction [Card])

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
  let south = sort $ map(snd) $ filter(\x -> (fst x) `mod` 4 == 0) helper
  let west  = sort $ map(snd) $ filter(\x -> (fst x) `mod` 4 == 1) helper
  let north = sort $ map(snd) $ filter(\x -> (fst x) `mod` 4 == 2) helper
  let east  = sort $ map(snd) $ filter(\x -> (fst x) `mod` 4 == 3) helper
  return (Board $ array(minBound, maxBound) [(South, south), (West, west), (North, north), (East, east)])
