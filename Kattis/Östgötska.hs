main :: IO ()
main = do
    inn <- fmap words getLine
    if sum (map östgötska inn) / fromIntegral (length inn) >= 0.4
        then putStrLn "dae ae ju traeligt va"
        else putStrLn "haer talar vi rikssvenska"

östgötska :: String -> Double
östgötska [] = 0
östgötska ('a':'e':_) = 1
östgötska (x:xs) = östgötska xs