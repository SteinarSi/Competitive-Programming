main :: IO ()
main = do
    xs <- map read . words <$> getLine
    let amount = maximum (zipWith (/) xs [12,12,40])
    putStrLn (unwords (zipWith (\x f -> show (f*amount - x)) xs [12,12,40]))
