import Cards
import Calls
import Auction hiding (turn)
import Game
import Player
import Conventions


import Control.Monad.Reader
import Control.Monad.State
import System.Process
import System.Console.ANSI

import System.IO.Unsafe (unsafePerformIO)
board' :: Board
board' = unsafePerformIO $ do
  mkBoard

-- s = getHand board' South
--
-- c = Contract {level = Calls.Two, strain = Trump Club, penalty = Just Double, dealer = prev South}

-- main = do
--   play <- openGame openingConvention c board'
--   -- print $ dummy play
--   print play
--
-- main = do
--   mapM (putStrLn) $ map (\x -> "\ESC[" ++ show x ++";5;16m" ++ " " ++ show x ++ "\ESC[0m") [0,1..255]


-- the true one
main :: IO ()
main = do

    board :: Board <- mkBoard
    start :: Direction <- randomDirection

    let auction :: Auction = mkAuction start

    contract :: Contract <- evalStateT (runAuction biddingConvention board) auction
    if contract == FourPasses
      then do
        print "XD"
      else do
        game :: Game <- openGame openingConvention contract board
        print game
