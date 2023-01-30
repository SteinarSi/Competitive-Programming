import Data.Maybe (fromJust)
import Data.List (find)

main :: IO ()
main = readFile "inputs/day2-input.txt" >>= print . foldr (\(x, y) (x', y') -> (x+x', y+y')) (0, 0) . map (\l -> (score l, scripted l)) . lines

scripted :: [Char] -> Int
scripted (a:_:'Y':_) = 3 + s a
scripted (a:_:'Z':_) = 6 + s (fromJust (find (\c -> elem (c, a) w) "ABC"))
scripted (a:_:'X':_) =     s (fromJust (find (\c -> elem (a, c) w) "ABC"))

score :: [Char] -> Int
score (a:_:b:_) | elem (t b, a) w = s (t b) + 6
                | elem (a, t b) w = s (t b)
                | otherwise       = s (t b) + 3
s :: Char -> Int
s 'A' = 1
s 'B' = 2
s 'C' = 3

t :: Char -> Char
t 'X' = 'A'
t 'Y' = 'B'
t 'Z' = 'C'

w :: [(Char, Char)]
w = [('A', 'C'), ('B', 'A'), ('C', 'B')]
