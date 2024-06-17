module Shuffle (shuffle) where
import System.Random (randomRIO)

-- private function.
swap :: [a] -> Int -> Int -> [a]
swap xs i j
  | i < length xs && j < length xs = 
      map (\(idx, elem) -> if idx == i then (xs !! j) else if idx == j then (xs !! i) else elem) $ zip [0..] xs

-- fisher-yates
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle [x] = return [x]
shuffle xs = do
    shuffled <- shuffle' xs (length xs - 1)
    return shuffled
  where
    shuffle' :: [a] -> Int -> IO [a]
    shuffle' xs 0 = return xs
    shuffle' xs n = do
        k <- randomRIO (0, n)
        let shuffled = swap xs k n 
        shuffle' shuffled (n - 1)
x :: [Int] = [0..10]
