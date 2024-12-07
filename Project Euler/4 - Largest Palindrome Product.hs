
main :: IO ()
main = print $ maximum [ x * y | x <- [999, 998..2], y <- [999, 998..2], palindrome (x*y)]

palindrome :: Int -> Bool
palindrome s = digits s == reverse (digits s)

digits :: Int -> [Int]
digits 0 = []
digits n = mod n 10 : digits (div n 10)