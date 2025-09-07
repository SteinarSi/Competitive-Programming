import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    n:_:xs <- getContents <&> (lines >>> map (words >>> last >>> read))

    let r = 36 - length xs

        combinations :: Int -> [Int] -> Int
        combinations m []     = r^m
        combinations m (x:xs) = sum (map (\i -> choose m i * combinations (m-i) xs) [0..min m x])

    print (combinations n xs)

choose :: Int -> Int -> Int
choose n k
    | k > n           = 0
    | k == 0          = 1
    | k > (n `div` 2) = n `choose` (n-k)
    | otherwise       = n * ((n-1) `choose` (k-1)) `div` k
