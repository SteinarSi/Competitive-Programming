import           Control.Monad (replicateM, when)
main :: IO ()
main = do
    n <- fmap read getLine
    when (n /= -1) $ do
        input <- fmap (map ((\[s,t] -> (s,t)) . map read . words)) (replicateM n getLine)
        let converted = zipWith (\(s1,t1) (_,t2) -> (s1, t1-t2)) input ((0,0) : input)
            distance  = sum $ map (uncurry (*)) converted
        putStrLn (show distance ++ " miles")
        main
