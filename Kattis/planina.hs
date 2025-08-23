main :: IO ()
main = getLine >>= print . planina . read

planina :: Integer -> Integer
planina 0 = 4
planina n = 3*4^(n-1) + 2^n + planina (n-1)
