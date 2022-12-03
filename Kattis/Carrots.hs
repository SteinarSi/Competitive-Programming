main :: IO ()
main = do 
    input <- getLine
    putStrLn ((words input)!!1)