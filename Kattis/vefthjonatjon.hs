main :: IO ()
main = interact $ show . minimum . foldr (zipWith (+) . map (\x -> if "J"==x then 1 else 0) . words) [0,0,0] . tail . lines
