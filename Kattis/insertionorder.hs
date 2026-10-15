import           Data.List (find)

main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine

    let Just fl = find (\l -> n <= 2^l-1+k-l) [1..]
        extras = n - (k-fl)

    putStrLn $ if extras > n
        then "impossible"
        else unwords (map show (distribute (1,extras) <> [extras+1..n]))

distribute :: (Int,Int) -> [Int]
distribute (from,to)
    | from > to = []
    | from == to = [from]
    | from+1 == to = [from,to]
    | otherwise = mi : distribute (from,mi-1) <> distribute (mi+1,to)
  where
    mi = (from+to) `div` 2
