import           Control.Arrow         ((&&&), (***), (>>>))
import           Data.Array.Unboxed    (Array, UArray, listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map (
                coalesce
            >>> (*100)
            >>> printf "%.6f\n")
        >>> concat
        >>> putStr
    )

coalesce :: (Int, UArray Int Int, UArray Int Double) -> Double
coalesce (n,seat,prob) = dp ! (n,0)
  where
    rng = ((0,0),(n,75))

    dp :: Array (Int,Int) Double
    dp = listArray rng (map f (range rng))

    f :: (Int,Int) -> Double
    f (0,_) = 0
    f (i,s) | s+c > 75  = max p (dp ! (i-1,s))
            | otherwise = max (p * dp ! (i-1,s + c)) (dp ! (i-1,s))
      where
        p = prob ! i
        c = seat ! i

parse :: [[Int]] -> [(Int, UArray Int Int, UArray Int Double)]
parse []       = []
parse ([n]:xs) = (n, seat, prob) : parse zs
  where
    seat = listArray (1,n) (map head ys)
    prob = listArray (1,n) (map (last >>> fromIntegral >>> (/100)) ys)
    (ys,zs) = splitAt n xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
