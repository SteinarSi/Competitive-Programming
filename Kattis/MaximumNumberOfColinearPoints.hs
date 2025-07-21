import           Control.Arrow         ((&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)
import           Data.Ratio            ((%))

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map (solve 0 >>> show)
        >>> unlines
        >>> putStr
    )

solve :: Int -> [(Int,Int)] -> Int
solve best []  = best
solve best [_] = max best 1
solve best ((x1,y1):xs) = map (normalize >>> hash >>> (,1)) xs
    & M.fromListWith (+)
    & M.elems
    & maximum
    & succ
    & max best
    & (`solve` xs)
  where
    normalize :: (Int,Int) -> (Int,Int)
    normalize (x,y)
        | dx < 0    = (-dx',-dy')
        | otherwise = ( dx', dy')
      where
        dx  = x-x1
        dy  = y-y1
        d   = gcd dx dy
        dx' = dx `div` d
        dy' = dy `div` d

hash :: (Int,Int) -> Int
hash (x,y) = x + 20001 * (y+10000)

parse :: [[Int]] -> [[(Int,Int)]]
parse [] = []
parse [[0]] = []
parse ([n]:xs) = splitAt n xs
    & map (head &&& last) *** parse
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
