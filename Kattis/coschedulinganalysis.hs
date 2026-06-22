import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> concatMap (C.words >>> drop 1 >>> map readInt >>> pairs)
        >>> sort
        >>> solve
        >>> print
    )

solve :: [(Int,Int)] -> Int
solve [] = 0
solve [_] = 0
solve ((px,py):(x,y):xs)
    | py <= x = solve ((x,y):xs)
    | y <= px = solve ((px,py):xs)
    | x <= px && px <= y && y <= py = y-px + solve ((y,py):xs)
    | px <= x && y <= py = y-x + solve ((y,py):xs)
    | px <= x && x <= py && py <= y = py-x + solve ((py,y):xs)
    | x <= px && py <= y = py-px + solve ((py,y):xs)
    | otherwise = error "no cases match :/"

pairs :: [Int] -> [(Int,Int)]
pairs []       = []
pairs (x:y:xs) = (x,y) : pairs xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
