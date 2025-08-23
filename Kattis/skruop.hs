main :: IO ()
main = interact (show . f 7 . tail . lines)

f :: Int -> [String] -> Int
f n [] = n
f n ("Skru op!" :xs) = f (min 10 (n+1)) xs
f n ("Skru ned!":xs) = f (max 0 (n-1)) xs