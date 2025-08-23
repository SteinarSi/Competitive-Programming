main :: IO ()
main = do
    xs <- fmap (map read . tail . words) getContents
    print (sum xs `div` length xs)
