import           Control.Arrow         (first, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (partition)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> partition (all (fst >>> (<=0)))
        >>> first (map (map snd))
        >>> bfs
        >>> print
    )

bfs :: ([[Int]], [[(Int,Int)]]) -> Int
bfs (valid,[]) = length valid
bfs ([],_) = 0
bfs (valid,unknown) = partition (\xs -> any (\ys -> and (zipWith (\(l1,_) b -> l1 <= b) xs ys)) valid) unknown
    & first (map (map snd))
    & bfs
    & (length valid +)

parse :: [[Int]] -> [[(Int,Int)]]
parse []          = []
parse (xs:ys:xss) = zip xs ys : parse xss

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
