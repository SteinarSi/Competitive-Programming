import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    n <- getLine <&> read
    let (den,num) = solve n 1 1 primes
        g = gcd den num
    putStrLn (show (den `div` g) <> "/" <> show (num `div` g))

solve :: Integer -> Integer -> Integer -> [Integer] -> (Integer,Integer)
solve n acc prod (p:ps) | prod*p > n = (prod-acc,prod)
                        | otherwise  = solve n (acc*(p-1)) (p*prod) ps

primes :: [Integer]
primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257]
