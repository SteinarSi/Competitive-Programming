main :: IO ()
main = do
    n <- read <$> getLine

    let p = soup !! min (n-1) 100

    putStrLn (show (100 * p) <> " " <> show (100 * (1-p)))

soup :: [Double]
soup = 1 : 0 : zipWith (\x y -> (x+y) / 2) soup (drop 1 soup)
