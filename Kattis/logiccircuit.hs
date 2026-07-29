main :: IO ()
main = do
    [a,b,c] <- (map (=="1") . words) <$> getContents
    print $ if (a && not b) || (not a && c)
        then 1
        else 0
