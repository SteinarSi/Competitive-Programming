import           Data.Bool (bool)

main :: IO ()
main = do
    k <- fmap read getLine
    people <- fmap (map (bool (-1) 1 . (=='M'))) getLine
    print $ doorman 0 k people

doorman :: Int -> Int -> [Int] -> Int
doorman curr k [] = 0
doorman curr k [x] | abs (curr+x) <= k = 1
                   | otherwise = 0
doorman curr k (x:y:xs) | x /= y = 2 + doorman curr k xs
                        | abs (curr + x) <= k = 1 + doorman (curr+x) k (y:xs)
                        | otherwise = 0
