import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n, m] <- getLine <&> (words >>> map read)

    let fs = factors m
        ds = divisors m
        mfs = filter (\f -> f <= n && m `div` f <= n) ds

    divisors m
        & filter (\f -> f <= n && m `div` f <= n)
        & length
        & print

divisors :: Int -> [Int]
divisors = factors >>> divs 1
    where
        divs :: Int -> [Int] -> [Int]
        divs x []     = [x]
        divs x (p:ps) = divs (x*p) ps <> divs x (filter (p/=) ps)

factors :: Int -> [Int]
factors = fac primes
    where
        fac _ 1 = []
        fac (p:ps) x
            | x `mod` p == 0 = p : fac (p:ps) (x `div` p)
            | p*p > x        = [x]
            | otherwise      = fac ps x

primes :: [Int]
primes = knownPrimes ++ filter prime [last knownPrimes + 2, last knownPrimes + 4 .. ]
    where
        prime :: Int -> Bool
        prime x = primes
            & takeWhile (<=sx)
            & all (mod x >>> (/=0))
          where
            sx = ceiling (sqrt (fromIntegral x))

        knownPrimes :: [Int]
        knownPrimes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
