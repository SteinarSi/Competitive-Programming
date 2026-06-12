import           Control.Arrow ((>>>))
import           Data.Bits     (countTrailingZeros, shiftR)
import           Data.Function ((&))

main :: IO ()
main = [(score a b, (a,b)) | b <- [2..1000], isPrime b, a <- [-999..999]]
    & maximum
    & snd
    & uncurry (*)
    & print

score :: Integer -> Integer -> Int
score a b = map (\n -> n*n + a*n + b) [0..]
    & takeWhile isPrime
    & length

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
