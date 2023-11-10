main :: IO ()
main = getLine >>= print . subtract 1 . read
