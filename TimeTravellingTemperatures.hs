main :: IO ()
main = do
    inn <- getLine
    let x = read $ words inn!!0
        y = read $ words inn!!1
    if y == 1 then 
        if x == 0 then putStrLn "ALL GOOD" 
        else putStrLn "IMPOSSIBLE"
    else print $ f y x

f :: Fractional a => a -> a -> a
f a b = b / (1-a)