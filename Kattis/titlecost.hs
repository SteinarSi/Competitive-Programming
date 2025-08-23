main :: IO ()
main = do
    t:c:_ <- fmap words getLine
    print $ min (fromIntegral (length t)) (read c :: Double)
