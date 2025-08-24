import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (foldl1')
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map (foldl1' overlap >>> area >>> show)
        >>> unlines
        >>> putStr
    )

area :: (Int,Int,Int,Int) -> Int
area (l,t,r,b) | l >= r || b >= t = 0
               | otherwise = (r-l) * (t-b)

overlap :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)
overlap (l1,t1,r1,b1) (l2,t2,r2,b2) = (
        max l1 l2,
        min t1 t2,
        min r1 r2,
        max b1 b2
    )

parse :: [[Int]] -> [[(Int,Int,Int,Int)]]
parse [] = []
parse ([n]:xs) = splitAt n xs
    & map (\[l,t,r,b] -> (l,t,r,b)) *** parse
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
