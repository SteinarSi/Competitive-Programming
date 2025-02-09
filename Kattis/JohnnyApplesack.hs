import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,k] <- getContents <&> (words >>> map read)
    print (solve k n)

solve :: Int -> Int -> Int
solve k n | n <= k    = n + 1
          | otherwise = 1 + solve k (n - ceiling (fromIntegral n / fromIntegral k))

