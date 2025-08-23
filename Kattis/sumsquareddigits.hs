
main :: IO ()
main = interact (unlines . map ((\(i:b:x:_) -> i ++ " " ++ show (squared (read b) (read x))) . words) . tail . lines)

squared :: Integer -> Integer -> Integer
squared _ 0 = 0
squared b x = (x `mod` b) ^ 2 + squared b (x `div` b)
