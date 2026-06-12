import           Control.Arrow ((>>>))
import           Data.Bits     (countTrailingZeros, shiftL, shiftR)
import           Data.Function ((&))
import           Data.List     (find)
import           Data.Maybe    (fromJust)

main :: IO ()
main = print (fromJust (find (divisors >>> length >>> (>=500)) triangles))

triangles :: [Integer]
triangles = map (\n -> (n*(n+1)) `div` 2) [1..]

divisors :: Integer -> [Integer]
divisors = factors >>> divs 1
    where
        divs :: Integer -> [Integer] -> [Integer]
        divs x []     = [x]
        divs x (p:ps) = divs (x*p) ps <> divs x (dropWhile (p==) ps)

factors :: Integer -> [Integer]
factors = fac primes
    where
        fac _ 1 = []
        fac (p:ps) x | x `mod` p == 0 = p : fac (p:ps) (x `div` p)
                     | p*p > x        = [x]
                     | otherwise      = fac ps x

primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

isPrime :: Integer -> Bool
isPrime n
    | n < 2 = False
    | n `elem` bases = True
    | any (mod n >>> (==0)) bases = False
    | otherwise = not (any (compositeCheck n) bases)
  where
    r = countTrailingZeros (fromInteger (n-1) :: Int)
    d = (n-1) `shiftR` r

    compositeCheck :: Integer -> Integer -> Bool
    compositeCheck n a = powmod a d n /= 1 && (n-1) `notElem` take r (iterate (\x' -> (x'*x') `mod` n) (powmod a d n))

    bases :: [Integer]
    bases = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]

powmod :: Integer -> Integer -> Integer -> Integer
powmod _ 0 _ = 1
powmod x n m
    | odd n     = (u'*x) `mod` m
    | otherwise = u'
  where
    u = powmod x (n `shiftR` 1) m
    u' = (u*u) `mod` m
