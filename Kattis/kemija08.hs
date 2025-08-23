main :: IO ()
main = interact decode

decode :: String -> String
decode [] = []
decode (x:'p':y:xs) | x == y && x `elem` "aeiou"  = x : decode xs
                    | otherwise = x : 'p' : decode (y:xs)
decode (x:xs) = x : decode xs