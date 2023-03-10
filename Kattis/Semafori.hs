main = do
    inn <- getLine
    let n = read (words inn !!0)
        meters = read (words inn !!1)
    lights <- mapM (const getLine) [1..n]
    print $ calculate (rework lights) 0 0 meters
    
rework :: [String] -> [(Int, Int, Int)]
rework [] = []
rework (x:xs) = let wrs = words x in (read $ wrs!!0, read $ wrs!!1, read $ wrs!!2) : rework xs

calculate :: [(Int, Int, Int)] -> Int -> Int -> Int -> Int
calculate [] t p m = t + m - p
calculate ((dd, r, g):ls) t p m = calculate ls (nyT + w) dd m
    where 
        nyT = t + dd-p
        w = max 0 (r - (mod nyT (r+g)))