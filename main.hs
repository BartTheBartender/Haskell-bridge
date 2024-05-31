import Cards
import Calls
import Auction
import Play

import Control.Monad.Random

main :: IO ()
main = do
  board <- evalRandIO mkBoard
  print board
