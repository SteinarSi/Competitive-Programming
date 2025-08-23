main :: IO ()
main = do
    getLine
    answers <- getLine

    let people  = [("Adrian", cycle "ABC"), ("Bruno", cycle "BABC"), ("Goran", cycle "CCAABB")]
        results = map (fmap (length . filter id . zipWith (==) answers)) people
        best    = maximum $ map snd results
        winners = [name | (name, score) <- results, score == best]

    print best
    mapM_ putStrLn winners
