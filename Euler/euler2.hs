main :: IO ()
main = print $ fibbo 1 2 0

fibbo :: Int -> Int -> Int -> Int
fibbo a b s | a >= 4000000 = s
            | even a = fibbo b (a+b) (a+s)
            | otherwise = fibbo b (a+b) s