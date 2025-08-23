main :: IO ()
main = getLine >>= \ant -> getLine >>= \al -> print $ average [read n | n <- words al, n /= "-1"]

average :: [Double]-> Double
average xs = (sum xs) / (fromIntegral $ length xs)