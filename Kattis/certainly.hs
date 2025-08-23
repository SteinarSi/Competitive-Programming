main :: IO ()
main = getLine >>= (print . count)

count :: String -> Int
count []                                       = 0
count ('c':'e':'r':'t':'a':'i':'n':'l':'y':xs) = 1 + count xs
count (_:xs)                                   = count xs
