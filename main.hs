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
board :: Board
board = unsafePerformIO $ do
    gen <- newStdGen
    return $ evalRand mkBoard gen

s = getHand board South

main = do
    gen <- newStdGen
    let board = evalRand mkBoard gen
    contract <- evalStateT (runAuction badConvention board) (mkAuction West)
    print contract
