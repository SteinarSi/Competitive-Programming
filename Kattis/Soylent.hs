main :: IO ()
main = getContents >>= mapM_ (print . ceiling . (/400) . read) . tail . words
