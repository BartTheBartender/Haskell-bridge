import System.Process

main :: IO ()
main = do
    putStrLn "Podaj pierwsza wartosc"
    x <- getLine
    putStrLn x
    putStrLn "Podaj druga wartosc"
    _ <- system "clear"
    y <- getLine
    putStrLn y

    -- Create a new mutable reference with an initial value of 0
    -- ref <- newIORef (0 :: Int)
    --
    -- -- Read the initial value
    -- initialValue <- readIORef ref
    -- putStrLn $ "Initial value: " ++ show initialValue
    --
    -- -- Clear the terminal
    -- _ <- system "clear"
    --
    -- -- Update the value
    -- writeIORef ref 42
    --
    -- -- Read the updated value
    -- updatedValue <- readIORef ref
    -- putStrLn $ "Updated value: " ++ show updatedValue
    --
