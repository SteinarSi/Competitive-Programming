main :: IO ()
main = do
    a <- fmap parse getLine
    b <- fmap parse getLine
    print ((b - a) `mod` (24*60))

parse :: String -> Int
parse [h1,h2,_,m1,m2] = 60 * read [h1,h2] + read [m1,m2]
