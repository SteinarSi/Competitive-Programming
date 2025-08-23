main :: IO ()
main = do
    (n:m:_) <- fmap (map read  . words) getLine
    let xs = map (prob n m) [2..n+m]
    mapM_ (print . snd) (filter (\(p, x) -> p == fst (maximum xs)) xs)

prob :: Int -> Int -> Int -> (Int, Int)
prob n m x = (length [() | a <- [1..n], b <- [1..m], a+b == x], x)