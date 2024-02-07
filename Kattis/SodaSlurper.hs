main :: IO ()
main = do
    e:f:c:_ <- fmap (map read . words) getLine
    print $ slurp (e+f) c

slurp :: Integer -> Integer -> Integer
slurp e c | m /= 0    = m + slurp (e `mod` c + m) c
          | otherwise = 0
    where m = e `div` c
