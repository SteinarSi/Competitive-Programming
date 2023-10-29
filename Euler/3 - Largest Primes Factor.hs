--Funker, men tregt

num :: Integer
num = 600851475143

main :: IO ()
main = print $ last $ factorize num

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve [ y | y <- xs, mod y x /= 0 ]

factorize :: Integer -> [Integer]
factorize p = fact p [2..round (sqrt (fromIntegral num :: Double))+1]

fact :: Integer -> [Integer] -> [Integer]
fact _ [] = []
fact p (d:ds) | d > p = []
              | mod p d == 0 = d : fact (div p d) (d:ds)
              | otherwise = fact p ds
