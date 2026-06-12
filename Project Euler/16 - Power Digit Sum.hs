import           Data.Bits (shiftL)

main :: IO ()
main = print (sum (digits (1 `shiftL` 1000)))

digits :: Integer -> [Integer]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)
