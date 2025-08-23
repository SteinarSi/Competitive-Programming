import           Control.Arrow         (first, (&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> concatMap (uncurry solve)
        >>> map (show >>> C.pack)
        >>> C.unlines
        >>> C.putStr
    )

solve :: [(Int,Int)] -> [(Int,Int)] -> [Int]
solve calls = map (\(s,t) -> length (filter (overlap (s,t)) calls))

overlap :: (Int,Int) -> (Int,Int) -> Bool
overlap (s,t) (a,b) = a < t && b > s

parse :: [[Int]] -> [([(Int,Int)],[(Int,Int)])]
parse []         = []
parse [_]        = []
parse ([n,m]:xs) = (calls,intervals) : parse c
    where
        (calls, b) = splitAt n xs
            & first (map (\[_,_,s,t] -> (s,s+t)))
        (intervals, c) = splitAt m b
            & first (map (\[s,t] -> (s,s+t)))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
