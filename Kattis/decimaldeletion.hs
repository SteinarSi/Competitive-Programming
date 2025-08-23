main :: IO ()
main = getLine >>= print . round . read
