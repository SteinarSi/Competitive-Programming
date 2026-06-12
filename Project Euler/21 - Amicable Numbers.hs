import           Control.Arrow ((>>>))
import           Control.Monad (guard)

main :: IO ()
main = print (sum amicable)

amicable :: [Int]
amicable = do
    x <- [1..10000]
    let dx = d x
    guard (x < dx && d dx == x)
    [x, dx]

d :: Int -> Int
d = factors >>> divs 1 >>> drop 1 >>> sum
    where
        divs :: Int -> [Int] -> [Int]
        divs x []     = [x]
        divs x (p:ps) = divs (x*p) ps <> divs x (dropWhile (p==) ps)

factors :: Int -> [Int]
factors = fac primes
    where
        fac _ 1 = []
        fac [] x = [x]
        fac (p:ps) x | x `mod` p == 0 = p : fac (p:ps) (x `div` p)
                     | p*p > x        = [x]
                     | otherwise      = fac ps x

primes :: [Int]
primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
