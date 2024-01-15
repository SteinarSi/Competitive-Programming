main :: IO ()
main = do
    rooms:teams:_ <- fmap (map read . words) getContents

    let (tally, rest) = quotRem teams rooms

    mapM_ putStrLn $
        replicate rest         (stars (tally+1)) ++
        replicate (rooms-rest) (stars tally)

stars :: Int -> String
stars x = replicate x '*'
