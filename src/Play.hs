module Play where
import Player
import Cards
import Calls

import Control.Monad.Reader
import Control.Monad.State
import Data.List (intercalate)

data Contract = Contract 
  {level :: Level, strain :: Strain, penalty :: Maybe Penalty, dealer :: Direction} | FourPasses deriving (Show, Eq)

type Trick = [Card]

data Game = Game {
  contract :: Contract,
  lead :: Direction, 
  board :: Board,
  currentTrick :: Trick,
  nofTricks :: Int}

instance Show Game where
  show game = undefined

dummy :: Game -> Direction
dummy = next.next.lead

turn :: Game -> Direction
turn game = last $ take ((length $ currentTrick $ game) + 1) $ iterate next (lead game)

result

trickWinner :: Game -> Maybe Direction
trickWinner game
  | length trick > 4 = error "the Play structure had > 4 cards in a trick!"
  | length trick == 4 = case strain $ contract $ game of
    NoTrump -> undefined
    Trump suit -> undefined
  | otherwise = Nothing
  where
  trick = currentTrick game

-- mkGame :: Contract -> Card -> Game
-- mkGame contract card = 

type Dummy = Hand
type OpeningConvention = ReaderT Contract (Reader Hand) Card
type PlayingConvention = ReaderT ([Trick], Dummy) (ReaderT Contract (Reader Hand)) Card

openGame :: OpeningConvention -> Contract -> Board -> IO Game
openGame openingConvention contract board = do
  let lead = next $ dealer $ contract
  let hand = getHand board lead
  case lead of
    South -> do
      card <- evalStateT (getCardFromPlayer hand) 0
      return $ Game contract lead board [card] 0
    _ -> do
      let card = runReader (runReaderT openingConvention contract) hand
      return $ Game contract lead board [card] 0

getCardFromPlayer :: Hand -> StateT Int IO Card
getCardFromPlayer hand@(Hand cards) = do
  index <- get
  lift $ mapM (\(idx, card) -> if idx == index 
    then do 
    putStr "\ESC[47m"
    putStr card
    putStr "\ESC[49m"
    else do 
      putStr card) 
    (zip [0,1..] $ map (\card -> show card ++ " ") cards)
  -- lift $ putStr "\nusage: a - move left; d - move right; x - choose card "
  mode <- lift $ getLine
  case mode of
    "a" -> do
      if index > 0 
        then do
          put (index - 1)
          getCardFromPlayer hand
        else do
          getCardFromPlayer hand
    "d" -> do
      if index < length cards - 1
        then do
          put (index + 1)
          getCardFromPlayer hand
        else do
          getCardFromPlayer hand
    "x" -> return $ cards !! index
    _ -> getCardFromPlayer hand

