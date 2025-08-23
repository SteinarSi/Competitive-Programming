import           Data.List (intercalate)

main :: IO ()
main = do
    getLine
    notes <- fmap (map parse . words) getLine
    let len = sum (map snd notes)

    mapM_ (\(d, c) -> putStrLn (c : ": " ++ intercalate [d] (line d c notes))) [
            (' ', 'G'),
            ('-', 'F'),
            (' ', 'E'),
            ('-', 'D'),
            (' ', 'C'),
            ('-', 'B'),
            (' ', 'A'),
            ('-', 'g'),
            (' ', 'f'),
            ('-', 'e'),
            (' ', 'd'),
            (' ', 'c'),
            (' ', 'b'),
            ('-', 'a')
        ]

parse :: String -> (Char, Int)
parse [c]    = (c, 1)
parse (c:xs) = (c, read xs)

line :: Char -> Char -> [(Char,Int)] -> [String]
line _ _ [] = []
line d c ((x,k):xs) = replicate k h : line d c xs
    where h | x == c    = '*'
            | otherwise = d
