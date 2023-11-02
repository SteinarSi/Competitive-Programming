main :: IO ()
main = interact (show . toDec . reverse . toBin . read) >> putChar '\n'

toBin :: Integer -> [Integer]
toBin 0 = []
toBin x = x `mod` 2 : toBin (x `div` 2)

toDec :: [Integer] -> Integer
toDec = sum . zipWith (*) (map (2^) [0..])