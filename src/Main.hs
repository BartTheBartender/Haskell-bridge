import Cards
import Calls
import Auction hiding (turn)
import Play
import Player
import BiddingSAYCSimplified


import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State

import System.Process
import System.IO.Unsafe (unsafePerformIO)

import System.Console.ANSI

board' :: Board
board' = unsafePerformIO $ do
    gen <- newStdGen
    return $ evalRand mkBoard gen

s = getHand board' South

main :: IO ()
main = do
  card <- evalStateT (getCardFromPlayer s) 0
  putStrLn $ "You selected: " ++ show card

printWithWhiteBackground :: String -> IO ()
printWithWhiteBackground str = do
    putStr "\ESC[47m"  -- Set background to white
    putStr str
    putStr "\ESC[49m"  -- Reset background color to default

-- main = do
--     gen <- newStdGen
--     let board = evalRand mkBoard gen
--     let start :: Direction
--         start = toEnum $ evalRand (getRandomR (0,3)) gen
--     contract <- evalStateT (runAuction simpleConvention board) (mkAuction start)
--     print contract


