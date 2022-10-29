import Control.Monad (replicateM_)

main :: IO ()
main = read <$> getLine >>= flip replicateM_ (count . map read . words <$> getLine >>= print)

count :: [Int] -> Int
count [] = 0
count [x] = 0
count (x:y:xs) = max 0 (y - 2*x) + count (y:xs)