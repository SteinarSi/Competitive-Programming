import           Control.Arrow         ((***), (>>>))
import           Data.Array.Base       (assocs, (!))
import           Data.Array.ST         (newArray, runSTUArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map (solve >>> show)
        >>> unlines
        >>> putStr
    )

solve :: (Int,Int,Int,[(Int,Int)]) -> Int
solve (c,r,0,_)       = c*r
solve (c,r,1,[(x,y)]) = c*r - r - c - (min r c - abs (x-y)) - (min r c - abs ((c-x+1)-y)) + 3
solve (c,r,n,xs)      = assocs cols
    & filter (snd >>> not)
    & map (\(x,_) -> length (filter (\y -> not (diag1 ! (x+y-2)) && not (diag2 ! (c-x+y-1))) rows))
    & sum
  where
    rows = (runSTUArray $ do
            ret <- newArray (1,r) False
            mapM_ (\(_,y) -> writeArray ret y True) xs
            pure ret)
        & assocs
        & filter (snd >>> not)
        & map fst

    cols = runSTUArray $ do
        ret <- newArray (1,c) False
        mapM_ (\(x,_) -> writeArray ret x True) xs
        pure ret

    diag1 = runSTUArray $ do
        ret <- newArray (0,r+c-2) False
        mapM_ (\(x,y) -> writeArray ret (x+y-2) True) xs
        pure ret

    diag2 = runSTUArray $ do
        ret <- newArray (0,r+c-2) False
        mapM_ (\(x,y) -> writeArray ret (c-x+y-1) True) xs
        pure ret

parse :: [[Int]] -> [(Int,Int,Int,[(Int,Int)])]
parse []        = []
parse [[0,0,0]] = []
parse ([c,r,n]:xs) = splitAt n xs
    & (map (\(x:y:_) -> (x,y)) >>> (c,r,n,)) *** parse
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
