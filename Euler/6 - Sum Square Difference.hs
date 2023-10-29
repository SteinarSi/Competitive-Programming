main :: IO ()
main = print (calc 100)

calc :: Integer -> Integer
calc n = (n*(n+1) `div` 2)^2 - sum (map (^2) [1..n])