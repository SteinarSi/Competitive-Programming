main :: IO ()
main = interact solve

solve :: String -> String
solve ""       = "\n"
solve ('B':xs) = "(ooo)" <> solve xs
solve (x:xs)   = x : solve xs
