main :: IO ()
main = do
    inn <- fmap lines getContents
    putStrLn $ case inn of
        [_,a]   -> a
        [_,_,_] -> "blandad best"
