main :: IO ()
main = do
    first  <- fmap (every 4) getLine
    second <- fmap (every 2) getLine
    third  <- fmap (every 4 . tail) getLine
    fourth <- fmap (every 2) getLine
    fifth  <- fmap (every 4) getLine

    let digits = parse first second third fourth fifth

    putStrLn $ case digits of
        Nothing -> "BOOM!!"
        Just xs | sum (zipWith (*) (reverse xs) (map (10^) [0..])) `mod` 6 == 0 -> "BEER!!"
                | otherwise -> "BOOM!!"

parse :: String -> String -> String -> String -> String -> Maybe [Int]
parse "" "" "" "" "" = Just []
parse ('*':as) ('*':'*':bs) (' ':cs) ('*':'*':ds) ('*':es) = (0 :) <$> parse as bs cs ds es
parse (' ':as) (' ':'*':bs) (' ':cs) (' ':'*':ds) (' ':es) = (1 :) <$> parse as bs cs ds es
parse ('*':as) (' ':'*':bs) ('*':cs) ('*':' ':ds) ('*':es) = (2 :) <$> parse as bs cs ds es
parse ('*':as) (' ':'*':bs) ('*':cs) (' ':'*':ds) ('*':es) = (3 :) <$> parse as bs cs ds es
parse ('*':as) ('*':'*':bs) ('*':cs) (' ':'*':ds) (' ':es) = (4 :) <$> parse as bs cs ds es
parse ('*':as) ('*':' ':bs) ('*':cs) (' ':'*':ds) ('*':es) = (5 :) <$> parse as bs cs ds es
parse ('*':as) ('*':' ':bs) ('*':cs) ('*':'*':ds) ('*':es) = (6 :) <$> parse as bs cs ds es
parse ('*':as) (' ':'*':bs) (' ':cs) (' ':'*':ds) (' ':es) = (7 :) <$> parse as bs cs ds es
parse ('*':as) ('*':'*':bs) ('*':cs) ('*':'*':ds) ('*':es) = (8 :) <$> parse as bs cs ds es
parse ('*':as) ('*':'*':bs) ('*':cs) (' ':'*':ds) ('*':es) = (9 :) <$> parse as bs cs ds es
parse as       bs           cs       ds           es       = Nothing

every :: Int -> [a] -> [a]
every a []     = []
every a (x:xs) = x : every a (drop (a-1) xs)
