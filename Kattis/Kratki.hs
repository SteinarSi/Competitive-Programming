import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,k] <- getLine <&> (words >>> map read)

    putStrLn $ if k^2 < n
        then "-1"
        else unwords (map show (solve k n))

solve :: Int -> Int -> [Int]
solve k n | n < 1     = []
          | otherwise = [max 1 (n-k+1) .. n] <> solve k (n-k)
