import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import qualified Data.IntSet           as S
import           Data.List             (minimumBy)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (readInt >>> solve >>> print)
    )

solve :: Int -> Int
solve k = morning 0 1
        & S.toList
        & minimumBy (compare `on` (subtract k >>> abs))
    where
        morning ::Int -> Int -> S.IntSet
        morning 0 0 = S.singleton 0
        morning r x | r > 2*k = S.singleton r
                    | otherwise = next x
                            & foldr (morning r >>> S.union) (morning (r * 10 + x) x)
                            & S.insert r

next :: Int -> [Int]
next 0 = []
next 1 = [2, 4]
next 2 = [3, 5]
next 3 = [6]
next 4 = [5, 7]
next 5 = [6, 8]
next 6 = [9]
next 7 = [8]
next 8 = [9, 0]
next 9 = []
next _ = error "bruh"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
