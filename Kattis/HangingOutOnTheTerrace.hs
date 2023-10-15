main :: IO ()
main = do
    l:_ <- fmap words getLine
    getContents >>= print . bounce 0 (read l) . map parse . lines

parse :: String -> Int
parse ('e':'n':'t':'e':'r':xs) =   read xs
parse ('l':'e':'a':'v':'e':xs) = - read xs

bounce :: Int -> Int -> [Int] -> Int
bounce _ _ [] = 0
bounce c l (x:xs) | c + x > l = 1 + bounce c l xs
                  | otherwise = bounce (c+x) l xs
