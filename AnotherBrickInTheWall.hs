main :: IO ()
main = do
    inn <- getLine
    let h = read $ words inn !! 0
        w = read $ words inn !! 1
    bricks <- getLine >>= return . (map read . words)
    if place bricks h w w then putStrLn "YES"
    else putStrLn "NO"

place :: [Int] -> Int -> Int -> Int -> Bool
place _ 0 _ _ = True
place [] _ _ _ = False
place (b:bs) layers width left | b > left  = False
                               | b == left = place bs (layers-1) width width
                               | otherwise = place bs layers width (left-b)