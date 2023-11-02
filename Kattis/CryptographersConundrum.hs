main :: IO ()
main = getLine >>= print . length . filter not  . zipWith (==) (cycle "PER")
