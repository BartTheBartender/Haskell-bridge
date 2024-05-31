data FixedList (n :: Nat) where
  FixedList :: Nat n => [Int] -> FixedList n
