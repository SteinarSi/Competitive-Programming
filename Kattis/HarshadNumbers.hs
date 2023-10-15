main :: IO ()
main = interact (show . harshad . read)

harshad :: Integer -> Integer
harshad x | x `mod` dsum x == 0 = x
          | otherwise           = harshad (succ x)

dsum :: Integer -> Integer
dsum 0 = 0
dsum x = x `mod` 10 + dsum (x `div` 10)
