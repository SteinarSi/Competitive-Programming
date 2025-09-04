import           Control.Arrow ((>>>))
import           Data.Array    (Array, listArray, (!))
import           Data.Char     (isSpace)
import           Data.Functor  ((<&>))

main :: IO ()
main = input "data.py" >>= (solve >>> print)

input :: String -> IO [Int]
input file = readFile file <&> (dropWhile (/='[') >>> filter (isSpace >>> not) >>> read)

solve :: [Int] -> Int
solve (x:y:xs) = arr ! n
    where
        n = length (x:y:xs)

        arr :: Array Int Int
        arr = listArray (1,2+length xs) $ x : max x y : zipWith (\i z -> max (arr ! (i-1)) (arr ! (i-2) + z)) [3..] xs
