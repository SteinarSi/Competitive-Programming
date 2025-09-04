import           Control.Arrow ((>>>))
import           Data.Char     (isSpace)
import           Data.Functor  ((<&>))

input :: String -> IO [Int]
input file = readFile file <&> (dropWhile (/='[') >>> filter (isSpace >>> not) >>> read)

solve :: [Int] -> Int
solve []     = 0
solve [x]    = x
solve (x:xs) = max (x + solve (drop 1 xs)) (solve xs)
