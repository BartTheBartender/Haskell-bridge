import Cards
import Calls
import Auction
import Play
import Player(Direction(..))

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State

main :: IO ()
main = do
    -- Generate a random seed
    gen <- newStdGen

    -- Create a random board
    let board = evalRand mkBoard gen
    print board

    -- Create an initial auction
    let initialAuction = mkAuction South

    -- Run the auction using badConvention
    resultContract <- evalStateT (runReaderT (runReaderT runAuction badConvention) board) initialAuction

    print resultContract

