module Player(Direction(..), next) where
import Data.Array

data Direction = South | West | North | East deriving (Show, Eq, Enum, Ord, Bounded, Ix)

next :: Direction -> Direction
next South = West
next West = North
next North = East
next East = South
