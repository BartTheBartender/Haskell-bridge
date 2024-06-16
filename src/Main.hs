module Main where
import Cards
import Auction hiding (turn)
import Game
import Player
import Conventions


import Control.Monad.State

main :: IO ()
main = do

    board :: Board <- mkBoard
    start :: Direction <- randomDirection

    let auction :: Auction = mkAuction start

    contract :: Contract <- evalStateT (runAuction biddingConvention board) auction
    if contract == FourPasses
      then do
        print contract
      else do
        game :: Game <- openGame openingConvention contract board
        result :: Int <- evalStateT (playGame playingConvention) game
        putStrLn $ "Score : " ++ show result
