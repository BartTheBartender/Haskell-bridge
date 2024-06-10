module Play where
import Player
import Cards
import Calls

import Control.Monad.Reader
import Control.Monad.State
import Data.List (intercalate, intersperse)
import Debug.Trace

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
  show game = line ++ "\n" ++ north ++ "\n" ++ vertical ++ south ++ "\n" ++ line where
  -- show game = show trick where

    stringifiedHands ::[[String]] = map (map align) $
      map (\(dir, cards) -> if dir == South || dir == dummy game
        then map show cards
        else map (const "\ESC[32;5;16m\x1F0A0\ESC[0m") cards) $
      map (\(dir, Hand hand) -> (dir,hand)) $
      map (\dir -> (dir, getHand (board game) dir)) [minBound..maxBound]

    align "" = "   "
    align card = card ++ (replicate (17 - length card) ' ')
    
    north = align "" ++
      (concat $ map align (
        (stringifiedHands !! 1) ++ (replicate 
        (13 - length (stringifiedHands !! 1)) "")
      )) ++ align ""
    south = align "" ++
      (concat $ map align (
        (stringifiedHands !! 3) ++ (replicate 
        (13 - length (stringifiedHands !! 3)) "")
      )) ++ align ""

    verticalRaw = take (max 
      (length $ stringifiedHands !! 0) 
      (length $ stringifiedHands !! 2)) $
      zip ((stringifiedHands !! 0) ++ replicate 13 "")
          ((stringifiedHands !! 2) ++ replicate 13 "")

    vertical = unlines $ map 
      (\((w,e),idx) -> case idx of
        2 ->
          w ++ concat (replicate 6 $ align "") ++ trick !! 1 ++ concat (replicate 6 $ align "") ++ e
        10 ->
          w ++ concat (replicate 6 $ align "") ++ trick !! 3 ++ concat (replicate 6 $ align "") ++ e
        6 ->
          w ++ concat (replicate 3 $ align "") ++ trick !! 0 ++
          concat (replicate 5 $ align "") ++ trick !! 2 ++
          concat (replicate 3 $ align "") ++ e

        _ ->
          w ++ concat (replicate 13 $ align "") ++ e) 
        $ zip verticalRaw [0,1..]

    line = replicate 43 '-'
    trick = map align $ reverse $ 
      shift n $ (replicate (4 - length curr) "") ++ map show curr where
      curr = currentTrick game
      n = fromEnum $ lead game
      shift 0 xs = xs
      shift n (x:xs) = shift (n-1) (xs ++ [x])

dummy :: Game -> Direction
dummy game = next.next.dealer $ contract game

turn' :: Game -> Direction
turn' game = last $ take ((length $ currentTrick $ game) + 1) $ iterate next (lead game)


--helper function. Assumes game is finished
score :: Game -> Int
score game = 0

trickWinner :: Game -> Maybe Direction
trickWinner game
  | length trick > 4 = error "the Play structure had > 4 cards in a trick!"
  | length trick == 4 = case strain $ contract $ game of
    NoTrump -> undefined
    Trump suit -> undefined
  | otherwise = Nothing
  where
  trick = currentTrick game
  lead' = lead game

availableCards :: Trick -> Hand -> [Card]
availableCards trick (Hand hand) = if null filtered then hand else filtered where
  filtered = if null trick then hand else filter (\card -> suit card == (suit $ last trick)) hand

advance :: Card -> Game -> Game
advance card game = 
  if elem card (availableCards trick (getHand board' turn''))
    then 
      game {currentTrick = (card:trick), board = (playCard board' turn'' card)} 
    else 
      error $ "The card " ++ show card ++ "is not available." 
    where
  turn'' = turn' game
  board' = board game
  trick = currentTrick game

type Dummy = Hand
type PlayingConvention = ReaderT Contract (Reader Hand) Card

badPlayingConvention :: PlayingConvention
badPlayingConvention = do
  hand <- lift ask
  return $ last $ availableCards [] hand

openGame :: PlayingConvention -> Contract -> Board -> IO Game
openGame convention contract board = do
  let lead = next $ dealer $ contract
  let hand = getHand board lead
  case lead of
    South -> do
      card <- evalStateT (getCardFromPlayer hand) 0
      return $ Game contract lead (playCard board lead card) [card] 0
    _ -> do
      let card = runReader (runReaderT convention contract) hand
      return $ Game contract lead (playCard board lead card) [card] 0


playGame :: ReaderT PlayingConvention (StateT Game IO) Int
playGame = do
  game <- lift get
  case trickWinner game of

    Just winner -> 
      if isEmpty (board game)
        then return (score game)
        else do
          let nofTricks' = if isPartner (lead game) winner 
                            then nofTricks game + 1 
                            else nofTricks game
          put $ game { lead = winner, nofTricks = nofTricks' }
          playGame

    Nothing -> do
      let turn'' = turn' game
          hand = getHand (board game) turn''
      lift $ lift $ print game
      if (isPartner turn'' South) && (isPartner (dealer $ contract game) South)
        then do
          let cardM :: (StateT Int IO) Card = do
                card <- getCardFromPlayer hand
                if elem card (availableCards (currentTrick game) hand)
                  then return card
                  else cardM
          card <- liftIO $ evalStateT cardM 0
          lift $ modify $ advance card
          playGame
        else do
          convention <- ask
          let card = runReader (runReaderT convention (contract game)) hand
          lift $ modify $ advance card
          playGame




getCardFromPlayer :: Hand -> StateT Int IO Card
getCardFromPlayer hand@(Hand cards) = do
  index <- get
  lift $ mapM (\(idx, card) -> if idx == index 
    then do 
    putStr $ "\ESC[47m" ++card
    putStr "\ESC[49m"
    else do 
      putStr card) 
    (zip [0,1..] $ map (\card -> show card ++ " ") cards)
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


