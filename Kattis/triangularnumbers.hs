main :: IO ()
main = do
    n <- fmap read getLine
    putStr (unlines (map (\x -> show ((x*(x+1))`div`2)) [1..n]))
