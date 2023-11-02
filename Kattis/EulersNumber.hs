import Text.Printf (printf)

main :: IO ()
main = getLine >>= printf "%.15f\n" . e . read

e :: Integer -> Double
e n = sum [ 1 / fromIntegral (fac i) | i <- [0..min n 40]]

fac :: Integer -> Integer
fac 0 = 1
fac k = k * fac (k-1)