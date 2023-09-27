main :: IO ()
main = do
    ((a, c):(b, d):_) <- fmap (map (split "") . lines) getContents
    putStrLn (a++b++"\n"++c++d)

split :: String -> String -> (String, String)
split s ('|':xs) = (reverse s, xs)
split s (x:xs) = split (x:s) xs