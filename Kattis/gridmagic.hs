import Data.Function ((&))
import Control.Arrow ((>>>))
import Control.Monad (guard)
import Data.Functor ((<&>))

main :: IO ()
main = do
    [n,m] <- getContents <&> (words >>> map read)
    print (length (solulu (m,n)))

solulu :: (Int,Int) -> [[Int]]
solulu (1,1) = map pure (prefixPrimes 1)
solulu (m,1) = prefixPrimes m
        & map digits
        & filter (all prime)
solulu (m,n) = do
        prevs <- solulu (m,n-1)
        p <- prefixPrimes m
        let next = zipWith (\row d -> row * 10 + d) prevs (digits p)
        guard (all prime next)
        pure next

digits :: Int -> [Int]
digits = ds >>> reverse
    where
        ds 0 = []
        ds x = x `mod` 10 : ds (x `div` 10)

prefixPrimes :: Int -> [Int]
prefixPrimes 0 = []
prefixPrimes 1 = [2,3,5,7]
prefixPrimes i = do
        p <- prefixPrimes (i-1)
        d <- [1,3,7,9]
        let q = 10 * p + d
        guard (prime q)
        pure q

primes :: [Int]
primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47] ++ filter prime [7,9..]

prime :: Int -> Bool
prime x 
    | x <= 1 = False
    | x == 2 = True
    | otherwise = primes
        & takeWhile (<=root)
        & all (mod x >>> (/=0))
    where
        root = fromIntegral x
            & sqrt
            & ceiling
