main :: IO ()
main = do
    n:d:xs <- fmap (map (read::String->Int) . words) getContents
    let years = length (takeWhile (>d) xs)
    putStrLn $ if years == n
        then "It had never snowed this early!"
        else "It hadn't snowed this early in " ++ show years ++ " years!"
