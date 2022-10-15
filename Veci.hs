main = do
    input <- getLine
    let større = [x | x<-combinations input, (read x::Int) > (read input::Int)]
    if større == [] then putStrLn "0"
    else putStrLn (minimum større)

combinations :: Eq a => [a] -> [[a]]
combinations xs = comb xs xs

comb :: Eq a => [a] -> [a] -> [[a]]
comb [] _ = []
comb _ [x] = [[x]]
comb (x:xs) as = (map (x:) (comb ns ns)) ++ comb xs as
    where ns = rem1 as x

rem1 :: Eq a => [a] -> a -> [a]
rem1 [] _       = []
rem1 (x:xs) b
    | b==x      = xs
    | otherwise = x : rem1 xs b