--Funker, men tregt

num :: Int
num = 600851475143

main :: IO ()
main = print $ [ y | y <- sieve [2..round (sqrt (fromIntegral num :: Double))], mod num y == 0]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve [ y | y <- xs, mod y x /= 0 ]