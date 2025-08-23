import           Control.Arrow ((>>>))
import qualified Data.IntSet   as S

main :: IO ()
main = getLine >>= (
            splitOn '+'
        >>> map read
        >>> nighteeneightyfour
        >>> S.size
        >>> print
    )

nighteeneightyfour :: [Int] -> S.IntSet
nighteeneightyfour []       = S.empty
nighteeneightyfour [x]      = S.singleton x
nighteeneightyfour (x:y:xs) = S.map (x +) (nighteeneightyfour (y:xs)) `S.union` nighteeneightyfour (x `plus` y : xs)

plus :: Int -> Int -> Int
plus a b = a * 10 ^ len b + b
    where
        len :: Int -> Int
        len 0 = 0
        len x = 1 + len (x `div` 10)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn p (x:xs) | p == x    = splitOn p xs
                 | otherwise = let (a, b) = span (p/=) xs
                               in  (x:a) : splitOn p b
