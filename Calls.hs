module Calls(Call(..), Level(..), Strain(..), Suit(..), Penalty(..)) where
import Cards (Suit(..))

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

  
data Level = One | Two | Three | Four | Five | Six | Seven deriving (Eq, Ord, Enum, Bounded)

data Penalty = Double | Redouble deriving (Eq, Ord, Enum, Bounded)
instance Show Penalty where
  show Double = "X"
  show Redouble = "XX"

instance Show Level where
  show Seven = show 7
  show Six   = show 6
  show Five  = show 5
  show Four  = show 4
  show Three = show 3
  show Two   = show 2
  show One   = show 1

data Call = Pass |Pen Penalty| Bid Level Strain deriving Eq

instance Show Call where
  show Pass = "Pass"
  show (Pen pen) = show pen
  show (Bid level strain) = show level ++ show strain
