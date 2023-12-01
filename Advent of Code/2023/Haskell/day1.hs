import Data.Char (isDigit)
import Data.List (isPrefixOf, find)
import Data.Maybe (maybe)

main :: IO ()
main = getContents >>= print . sum . map ((\s -> 10 * head s + last s) . parse) . lines

parse :: String -> [Int]
parse "" = []
parse (x:xs) | isDigit x = read [x] : parse xs
parse xs = maybe [] (pure . snd) (find ((`isPrefixOf` xs) . fst) numbers) ++ parse (tail xs)

numbers :: [(String, Int)]
numbers = [
        ("zero", 0),
        ("one", 1),
        ("two", 2),
        ("three", 3),
        ("four", 4),
        ("five", 5),
        ("six", 6),
        ("seven", 7),
        ("eight", 8),
        ("nine", 9)
    ]
