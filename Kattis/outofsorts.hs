import           Control.Arrow         (second, (>>>))
import           Data.Array.Base       (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> map readInt
        >>> generate
        >>> possible
        >>> print
    )

possible :: [Int] -> Int
possible xs = poss (1,length xs) (zip [1..] xs)
        & S.fromList
        & S.size
    where
        poss :: (Int,Int) -> [(Int,Int)] -> [Int]
        poss _       []      = []
        poss (lo,hi) ys | hi <= lo  = map snd ys
                        | otherwise = lt' <> eq' <> gt'
            where
                mid = (lo+hi) `div` 2
                pivot = arr ! mid

                (lt,(eq,gt)) = span (fst >>> (<mid)) ys
                    & second (span (fst >>> (==mid)))

                lt' = filter (snd >>> (<pivot)) lt
                    & poss (lo,mid-1)
                gt' = filter (snd >>> (>pivot)) gt
                    & poss (mid+1,hi)
                eq' = map snd eq
                    & filter (==pivot)

        arr :: UArray Int Int
        arr = listArray (1,length xs) xs

generate :: [Int] -> [Int]
generate [n,m,a,c,x0] = iterate (\x -> (a*x + c) `mod` m) x0
        & drop 1
        & take n

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
