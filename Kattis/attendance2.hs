main :: IO ()
main = do
    roll <- fmap (missing . tail . lines) getContents
    putStrLn $ case roll of
        [] -> "No Absences"
        xs -> unlines xs

missing :: [String] -> [String]
missing []                = []
missing [x]               = [x]
missing (_:"Present!":xs) = missing xs
missing (x:y:xs)          = x : missing (y:xs)
