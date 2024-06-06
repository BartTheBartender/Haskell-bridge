module Player where
import Data.Array

data Direction = West | North | East | South deriving (Show, Eq, Enum, Ord, Bounded, Ix)

next :: Direction -> Direction
next West = North
next North = East
next East = South
next South = West

prev :: Direction -> Direction
prev West = South
prev North = West
prev East = North
prev South = East

isPartner :: Direction -> Direction -> Bool
isPartner direction direction' = direction' == direction || direction' == next (next direction)

isOpponent :: Direction -> Direction -> Bool
isOpponent = (not .) . isPartner
