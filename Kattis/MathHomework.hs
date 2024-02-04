main :: IO ()
main = do
    xs <- fmap (map read . words) getLine
    putStrLn $ case combs (init xs) (last xs) of
        [] -> "impossible"
        cs -> unlines cs

combs :: [Int] -> Int -> [String]
combs [] 0     = [""]
combs [] l     = []
combs (x:xs) l = concatMap (\d -> map ((show d ++ " ") ++) (combs xs (l - x*d))) [0..l `div` x]
