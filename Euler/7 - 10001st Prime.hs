main :: IO ()
main = print $ sieve [2..] !! 10000

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve [ y | y <- xs, mod y x /= 0 ]