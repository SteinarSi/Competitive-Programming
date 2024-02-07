main :: IO ()
main = do
    rows <- fmap lines getContents
    let cols = map (\i -> map (!!i) rows) [0..2]
        diag = [map (\i -> rows !! i !! i) [0..2], map (\i -> rows !! (2-i) !! i) [0..2] ]

    putStrLn $ if "OOO" `elem` rows ++ cols ++ diag
        then "Jebb"
        else "Neibb"
