import Control.Applicative (liftA2)

main :: IO ()
main = liftA2 (yoda [] []) get get >>= \(n, m) -> putStrLn n >> putStrLn m
    where get = fmap read getLine

yoda :: [Int] -> [Int] -> Int -> Int -> (String, String)
yoda xs ys 0 0 = (digitsToYoda xs, digitsToYoda ys)
yoda xs ys n m | ln < lm   = yoda  xs     (lm:ys) (div n 10) (div m 10)
               | ln > lm   = yoda (ln:xs)  ys     (div n 10) (div m 10)
               | otherwise = yoda (ln:xs) (lm:ys) (div n 10) (div m 10)
    where ln = mod n 10
          lm = mod m 10

digitsToYoda :: [Int] -> String
digitsToYoda [] = "YODA"
digitsToYoda xs = show . sum . expoMap (10*) $ reverse xs

expoMap :: (a -> a) -> [a] -> [a]
expoMap _ [] = []
expoMap f (x:xs) = x : expoMap f (map f xs)