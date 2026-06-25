main :: IO ()
main = do
    xs <- getLine
    putStrLn (swap xs (reverse (filter isVowel xs)))

swap :: String -> [Char] -> String
swap xs [] = xs
swap (x:xs) (v:vs)
    | isVowel x = v : swap xs vs
    | otherwise = x : swap xs (v:vs)

isVowel :: Char -> Bool
isVowel = (`elem` "aeiouAEIOU")
