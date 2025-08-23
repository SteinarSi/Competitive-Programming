main :: IO ()
main = do
    [x, y, r] <- fmap (map read . words) getContents
    printen (x-r) (y-r)
    printen (x+r) (y-r)
    printen (x+r) (y+r)
    printen (x-r) (y+r)
    where printen x y = putStrLn (show x ++ " " ++ show y)
