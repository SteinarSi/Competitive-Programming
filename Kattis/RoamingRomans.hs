main :: IO ()
main = getLine >>= print . round . (*(1000 * 5280 / 4854)) . read
