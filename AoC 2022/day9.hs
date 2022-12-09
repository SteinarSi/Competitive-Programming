import Data.Set hiding (map)

main :: IO ()
main = do
    inn <- fmap (map (\[[c],n] -> (c, read n)) . map words . lines) (readFile "day9-input.txt")
    print (size $ simul (0,0) [(0,0)] inn, size $ simul (0,0) (replicate 9 (0,0)) inn)

simul :: (Int, Int) -> [(Int, Int)] -> [(Char, Int)] ->  Set (Int, Int)
simul _    _     []         = singleton (0,0)
simul head chain ((_,0):xs) = simul head chain xs
simul head chain ((c,n):xs) = insert (last chain') $ simul nh chain' ((c,n-1):xs)
    where chain' = whip nh chain
          nh = move c head
    
distant :: (Int, Int) -> (Int, Int) -> Bool
distant (tx, ty) (hx, hy) = abs (tx - hx) > 1 || abs (ty - hy) > 1

whip :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
whip _ [] = []
whip h@(hx,hy) (t@(tx,ty):ts) | distant t h = nt : whip nt ts
                              | otherwise = t : whip t ts
    where nt = (tx + signum (hx-tx), ty + signum (hy-ty))

move :: Char -> (Int, Int) -> (Int, Int)
move 'R' (x,y) = (x+1,y)
move 'L' (x,y) = (x-1,y)
move 'U' (x,y) = (x,y-1)
move 'D' (x,y) = (x,y+1)
