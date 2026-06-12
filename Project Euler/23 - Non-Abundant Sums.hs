import           Control.Arrow ((>>>))
import           Data.Function ((&))
import qualified Data.IntSet   as S

main :: IO ()
main = [1..limit]
    & filter (`S.notMember` abundantSums)
    & sum
    & print

abundantSums :: S.IntSet
abundantSums = S.fromList $ do
    a <- abundants
    b <- abundants
    pure (a+b)

abundants :: [Int]
abundants = filter abundant [1..limit]

abundant :: Int -> Bool
abundant x = x < sum (drop 1 (divs 1 (factors x)))
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

limit :: Int
limit = 28123

primes :: [Int]
primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173]
