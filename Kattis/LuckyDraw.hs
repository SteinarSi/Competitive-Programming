import           Data.Functor ((<&>))
import           Text.Printf  (printf)

main :: IO ()
main = do
    [n',k',p'] <- getLine <&> words
    let (n,k,p) = (read n', read k', read p')

        solve :: Double -> Double -> Integer -> Double
        solve ret lost round
            | round >= 2000 = 1 - fromIntegral n * ret
            | otherwise     = solve (ret + loss * lost^^(n-1)) (lost+loss) (round+1)
          where
            loss :: Double
            loss = fromIntegral (choose (round-1) (k-1)) * (1-p)^^k * p^^(round-k)

    printf "%.6f\n" (solve 0 0 k)

choose :: Integer -> Integer -> Integer
choose n k | k > n           = 0
           | k == 0          = 1
           | k > (n `div` 2) = n `choose` (n-k)
           | otherwise       = n * ((n-1) `choose` (k-1)) `div` k
