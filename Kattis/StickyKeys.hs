main :: IO ()
main = interact unstick >> putChar '\n'

unstick :: String -> String
unstick (x:y:xs) | x == y = unstick (y:xs)
                 | otherwise = x : unstick (y:xs)
unstick xs = xs