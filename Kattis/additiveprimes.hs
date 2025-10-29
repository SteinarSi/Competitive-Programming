{-# LANGUAGE MultiWayIf #-}

import Control.Arrow ((>>>))

main :: IO ()
main = do
    n <- fmap read getLine

    putStrLn $ if
        | not (prime n) -> "COMPOSITE"
        | prime (digitsum n) -> "ADDITIVE PRIME"
        | otherwise -> "PRIME, BUT NOT ADDITIVE"

digitsum :: Int -> Int
digitsum 0 = 0
digitsum n = let (q,r) = quotRem n 10
             in  r + digitsum q

prime :: Int -> Bool
prime 0 = False
prime 1 = False
prime 2 = True
prime x = all (mod x >>> (/=0)) (takeWhile ((<=sq)) primes)
  where
    sq = ceiling (sqrt (fromIntegral x))

primes :: [Int]
primes = knownPrimes ++ filter prime [last knownPrimes + 2, last knownPrimes + 4 .. ]
  where
    knownPrimes :: [Int]
    knownPrimes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
