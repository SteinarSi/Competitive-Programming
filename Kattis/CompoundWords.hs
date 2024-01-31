import           Data.List (sort)

main :: IO ()
main = do
    xs <- fmap words getContents
    mapM_ putStrLn $ deleteDuplicates $ sort [ x ++ y | x <- xs, y <- xs, x /= y ]

deleteDuplicates :: [String] -> [String]
deleteDuplicates [] = []
deleteDuplicates [x] = [x]
deleteDuplicates (x:y:xs) | x == y = deleteDuplicates (y:xs)
                          | otherwise = x : deleteDuplicates (y:xs)
