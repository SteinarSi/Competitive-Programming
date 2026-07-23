import           Control.Arrow         ((***), (>>>))
import           Data.Array.ST         (newArray, readArray, runSTUArray,
                                        writeArray)
import           Data.Array.Unboxed    (elems, range, rangeSize, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map C.words
        >>> parse
        >>> map solve
        >>> unlines
        >>> putStr
    )

solve :: (Int,Int,[(String,[(Int,Int)])]) -> String
solve (w,h,xs) = unlines (total : unallocated : contested : allocated)
  where
    total = printf "Total %d" (rangeSize rng)
    unallocated = elems grid
        & filter (==0)
        & length
        & printf "Unallocated %d"
    contested = elems grid
        & filter (>=2)
        & length
        & printf "Contested %d"
    allocated = map (\(n,r) -> printf "%s %d" n (length (filter ((grid !) >>> (<=1)) r))) xs

    rng = ((1,1),(w,h))
    grid = runSTUArray $ do
        ret <- newArray rng (0::Int)
        mapM_ (snd >>> mapM_ (\v -> readArray ret v >>= (succ >>> writeArray ret v))) xs
        pure ret

parse :: [[C.ByteString]] -> [(Int,Int,[(String,[(Int,Int)])])]
parse [] = []
parse ([w,h]:[n]:xs) = splitAt (readInt n) xs
    & (map (\(n:cs) -> (C.unpack n, map readInt cs & \[x1,y1,x2,y2] -> range ((x1+1,y1+1),(x2,y2)))) >>> (readInt w, readInt h, )) *** parse
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
