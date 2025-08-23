import Control.Monad (replicateM_)

main :: IO ()
main = getLine >>= flip replicateM_ (getLine >>= print . count . map read . words) . read

count :: [Int] -> Int
count [] = 0
count [x] = 0
count (x:y:xs) = max 0 (y - 2*x) + count (y:xs)