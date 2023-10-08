main :: IO ()
main = interact (solve . head) 

solve :: Char -> String
solve s | elem s "AEIOU" = "Jebb"
        | s == 'Y' = "Kannski"
        | otherwise = "Neibb"