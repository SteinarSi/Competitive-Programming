main :: IO ()
main = do
    a:b:b2:_ <- fmap (map read . tail . lines) getContents
    putStrLn (classify a b b2)

classify :: Int -> Int -> Int -> String
classify a b b2 | a >= b = "VEIT EKKI"
                | b2 < 0 = "JEDI"
                | b2 > 0 = "SITH"
