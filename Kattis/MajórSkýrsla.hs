import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,k] <- getContents <&> (words >>> map read)

    important k (n-1)
        & reverse
        & order [n,n-1..] [1..] 1
        & take n
        & map show
        & unwords
        & putStrLn

order :: [Int] -> [Int] -> Int -> [Int] -> [Int]
order _ lt _ [] = lt
order (g:gt) (l:lt) i (x:xs) | i == x    = g : order gt     (l:lt) (i+1) xs
                             | otherwise = l : order (g:gt) lt     (i+1) (x:xs)

important :: Int -> Int -> [Int]
important 0 _ = []
important k i | k >= i    = i : important (k-i) (i-1)
              | otherwise =     important k     (i-1)
