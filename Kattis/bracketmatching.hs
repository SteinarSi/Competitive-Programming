main :: IO ()
main = getLine >> getLine >>= putStrLn . validate ""

validate :: String -> String -> String
validate "" "" = "Valid"
validate _  "" = "Invalid"
validate stack ('(':xs) = validate (')':stack) xs
validate stack ('[':xs) = validate (']':stack) xs
validate stack ('{':xs) = validate ('}':stack) xs
validate "" (_:_) = "Invalid"
validate (s:stack) (x:xs) | x == s = validate stack xs
                          | otherwise = "Invalid"
