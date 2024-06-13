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
import System.Random (newStdGen)

import System.IO.Unsafe (unsafePerformIO)
board' :: Board
board' = unsafePerformIO $ do
  mkBoard

s = getHand board' South

c = Contract {level = Calls.Two, strain = Trump Club, penalty = Just Double, dealer = prev South}

main :: IO ()
main = do
  play <- openGame badOpeningConvention c board'
  -- print $ dummy play
  print play
--
-- main = do
--   mapM (putStrLn) $ map (\x -> "\ESC[" ++ show x ++";5;16m" ++ " " ++ show x ++ "\ESC[0m") [0,1..255]


-- main = do
--     gen <- newStdGen
--     let board = evalRand mkBoard gen
--     let start :: Direction
--         start = toEnum $ evalRand (getRandomR (0,3)) gen
--     contract <- evalStateT (runAuction simpleConvention board) (mkAuction start)
    -- print contract


