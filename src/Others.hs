module Others (clear)
where

clear :: IO ()
clear = mapM_ (const $ putChar '\b') [0..2000]

greetings = "Welcome to the bridge game"
