main :: IO ()
main = interact ((\(_:k:xs) -> unwords (zoom (read k) (read k) xs)) . words)

zoom :: Int -> Int -> [a] -> [a]
zoom _ _ []     = []
zoom k 1 (x:xs) = x : zoom k k xs
zoom k j (x:xs) = zoom k (pred j) xs
