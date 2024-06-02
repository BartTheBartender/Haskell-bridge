module Calls(Call(..), Level(..), Strain(..), Suit(..), Penalty(..)) where
import Cards (Suit(..))

import Text.Read

data Strain = Trump Suit | NoTrump deriving (Eq, Ord)

instance Enum Strain where
  toEnum :: Int -> Strain
  toEnum n
    | n >= 0 && n <= 3 = Trump (toEnum n)
    | n == 4 = NoTrump
    | otherwise = error "Invalid enum value for Strain"

  fromEnum :: Strain -> Int
  fromEnum (Trump suit) = fromEnum suit
  fromEnum NoTrump = 4

instance Bounded Strain where
  minBound = Trump Club
  maxBound = NoTrump

instance Show Strain where
  show NoTrump = "NT"
  show (Trump Club) = "C"
  show (Trump Diamond) = "D"
  show (Trump Heart) = "H"
  show (Trump Spade) = "S"

instance Read Strain where
  readPrec = parens $ do
    Ident str <- lexP
    case str of
      "C"  -> return $ Trump Club
      "D"  -> return $ Trump Diamond
      "H"  -> return $ Trump Heart
      "S"  -> return $ Trump Spade
      "NT" -> return NoTrump
      _    -> pfail 

data Level = One | Two | Three | Four | Five | Six | Seven deriving (Eq, Ord, Enum, Bounded)

instance Show Level where
  show Seven = show 7
  show Six   = show 6
  show Five  = show 5
  show Four  = show 4
  show Three = show 3
  show Two   = show 2
  show One   = show 1

instance Read Level where
  readPrec = toLevel <$> readPrec where 
    toLevel 1 = One
    toLevel 2 = Two
    toLevel 3 = Three
    toLevel 4 = Four
    toLevel 5 = Five
    toLevel 6 = Six
    toLevel 7 = Seven
    toLevel _ = error "This is not a correct level"

data Penalty = Double | Redouble deriving (Eq, Ord, Enum, Bounded)

instance Show Penalty where
  show Double = "X"
  show Redouble = "XX"

instance Read Penalty where
  readPrec = parens $ do
    Ident str <- lexP
    case str of
      "X"  -> return Double
      "XX" -> return Redouble
      _    -> pfail

data Call = Pass |Pen Penalty| Bid Level Strain deriving Eq

instance Show Call where
  show Pass = "Pass"
  show (Pen pen) = show pen
  show (Bid level strain) = show level ++ show strain

instance Read Call where
  readPrec = parens $ choice
    [ do
        Ident "Pass" <- lexP
        return Pass
    , do
        pen <- readPrec
        return (Pen pen)
    , do
        level <- readPrec
        strain <- readPrec
        return (Bid level strain)
    ]
