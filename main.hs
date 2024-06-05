import Cards
import Calls
import Auction
import Play
import Player
import SAYC


import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State

import System.Process
import System.IO.Unsafe (unsafePerformIO)

import System.Console.ANSI

board :: Board
board = unsafePerformIO $ do
    gen <- newStdGen
    return $ evalRand mkBoard gen

s = getHand board South

main = do
    gen <- newStdGen
    let board = evalRand mkBoard gen
    let start :: Direction
        start = toEnum $ evalRand (getRandomR (0,3)) gen
    contract <- evalStateT (runAuction simpleConvention board) (mkAuction start)
    print contract





