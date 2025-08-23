import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.Maybe    (fromJust)

main :: IO ()
main = do
    k <- getLine <&> read
    let xs = takeWhile (<=k) fib
            & reverse
    solve xs k
        & reverse
        & map show
        & unwords
        & putStrLn

solve :: [Int] -> Int -> [Int]
solve _ 0 = []
solve (x:xs) k | x > k = solve xs k
               | otherwise = x : solve (x:xs) (k-x)

fib :: [Int]
fib = 0 : 1 : zipWith (+) fib (tail fib)
