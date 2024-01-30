main :: IO ()
main = do
    xs <- fmap (map read . words) getContents
    let (b1,b2) = head [(x, y) | x <- xs, y <- xs, x /= y, x + y == sum xs - 100]
    mapM_ print $ filter (`notElem` [b1,b2]) xs
