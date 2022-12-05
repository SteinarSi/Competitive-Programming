main :: IO ()
main = do
    f <- readFile "day5-input.txt"
    let initial = map (++[' ']) $ parse 0 (replicate 9 []) $ take 287 f
    print (simulate initial initial (map words $ lines $ drop 325 f))

type Crates = [[Char]]

simulate :: Crates -> Crates -> [[String]] -> (String, String)
simulate c9000 c9001 [] = (map head c9000, map head c9001)
simulate c9000 c9001 ((_:amount:_:from:_:to:_):xs) = simulate (iterate (move f t) c9000 !! a) (moveN f t a c9001) xs
    where f = read from - 1
          t = read to - 1
          a = read amount

parse :: Int -> Crates -> String -> Crates
parse _ acc []     = map reverse acc
parse 9 acc (_:xs) = parse 0 acc xs
parse n acc xs     = case xs of
    ' ':' ':' ':' ':ys -> parse (n+1) acc ys
    ' ':' ':' ':    ys -> parse (n+1) acc ys
    '[': c :']':' ':ys -> parse (n+1) (modify n (c:) acc) ys
    '[': c :']':    ys -> parse (n+1) (modify n (c:) acc) ys

modify :: Int -> (a -> a) -> [a] -> [a]
modify _ _ []     = []
modify 0 f (x:xs) = f x : xs
modify n f (x:xs) = x : modify (n-1) f xs

move :: Int -> Int -> Crates -> Crates
move i j crates = modify j (head (crates !! i) :) $ modify i tail crates

moveN :: Int -> Int -> Int -> Crates -> Crates
moveN i j amount crates = modify j (take amount (crates!!i)++) $ modify i (drop amount) crates

