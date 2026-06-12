import           Data.Function ((&))

main :: IO ()
main = fac 100
    & digits
    & sum
    & print

fac :: Integer -> Integer
fac n = product [1..n]

digits :: Integer -> [Integer]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)
