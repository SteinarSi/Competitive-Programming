import           Control.Arrow ((>>>))
import           Text.Printf   (printf)

main :: IO ()
main = getContents >>= (
        words
    >>> map read
    >>> search
    >>> putStrLn)

search :: [Double] -> String
search [n,p,s,v] = let c = bin 50 0.0000001 1000
                   in  printf "%.6f %.6f" (time c) c
    where
        bin :: Int -> Double -> Double -> Double
        bin i lo hi | i <= 0                        = c
                    | time c <= time (c + 0.000001) = bin (i-1) lo c
                    | otherwise                     = bin (i-1) c hi
            where c = (lo + hi) / 2

        time :: Double -> Double
        time c = let flytime = s * (1 + 1/c) / v
                     runtime = (n * logBase 2 n**(c*sqrt 2)) / (p * fromIntegral (10^9))
                 in  flytime + runtime
