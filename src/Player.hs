module Player where
import Data.Array
import System.Random (randomRIO)


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

partner :: Direction -> Direction
partner = next.next

isPartner :: Direction -> Direction -> Bool
isPartner direction direction' = direction' == direction || (direction' == partner direction)

isOpponent :: Direction -> Direction -> Bool
isOpponent = (not .) . isPartner

randomDirection :: IO Direction
randomDirection = do
  randomNumber <- randomRIO (0,3)
  return $ toEnum randomNumber
