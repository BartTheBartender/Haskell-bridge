module Player(Direction(..), next, prev, isPartner, isOpponent) where
import Data.Array

data Direction = South | West | North | East deriving (Show, Eq, Enum, Ord, Bounded, Ix)

next :: Direction -> Direction
next South = West
next West = North
next North = East
next East = South


prev :: Direction -> Direction
prev South = East
prev West = South
prev North = West
prev East = North

isPartner :: Direction -> Direction -> Bool
isPartner direction direction' = direction' == direction || direction' == next (next direction)

isOpponent :: Direction -> Direction -> Bool
isOpponent = (not .) . isPartner
