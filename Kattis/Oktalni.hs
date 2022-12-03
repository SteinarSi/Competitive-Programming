import Data.Char

main :: IO ()
main = getLine >>= \s -> putStrLn $ removeSpaces $ take3 (addSpaces s)

take3 :: String -> String
take3 x
    | length x > 0 = show (octo (take 3 x)) ++ take3 (drop 3 x)
    | otherwise = ""

addSpaces :: [Char] -> [Char]
addSpaces s = replicate (3 - length s `mod` 3) '0' ++ s

removeSpaces :: [Char] -> [Char]
removeSpaces ('0':[]) = "0"
removeSpaces ('0':xs) = removeSpaces xs
removeSpaces x = x

octo :: String -> Int
octo (x:y:z:[]) = 4*(digitToInt x) + octo (y:[z])
octo (y:z:[]) = 2*(digitToInt y) + octo [z]
octo (z:[]) = digitToInt z
octo _ = error "bruh"