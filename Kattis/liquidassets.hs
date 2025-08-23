import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (
            words
        >>> drop 1
        >>> map (doubles >>> vowels)
        >>> unwords
        >>> putStrLn
    )

vowels :: String -> String
vowels xs | length xs <= 2 = xs
          | otherwise      = head xs : filter (`notElem` "aeiou") (init (tail xs)) <> [last xs]

doubles :: String -> String
doubles [] = []
doubles [x] = [x]
doubles (x:y:xs) | x == y    =     doubles (y:xs)
                 | otherwise = x : doubles (y:xs)
