import           Control.Arrow         ((>>>))
import           Data.Array            (Array)
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (elemIndex)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.readInt
        >>> fromJust
        >>> fst
        >>> solve
        >>> print
    )

solve :: Int -> Int
solve p = memo ! 0
    where
        Just n = elemIndex p distances

        memo :: Array Int Int
        memo = listArray (0,n) $ map f [0..n]
            where
                f i | i == n    = 1
                    | otherwise = [i+1..]
                        & takeWhile (<=n)
                        & filter ((distance !) >>> subtract (distance ! i) >>> (<=14))
                        & map (memo !)
                        & sum

distance :: UArray Int Int
distance = listArray (0,length distances-1) distances

distances :: [Int]
distances = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211]
