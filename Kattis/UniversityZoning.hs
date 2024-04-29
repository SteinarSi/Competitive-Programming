import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap           as M
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [r, c, f, s, g] <- C.getLine <&> (C.words >>> map readInt)

    lll <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    let facs = take f lll
            & map (tail >>> toTuples >>> sort)
        students = drop f lll
            & take s
            & foldr (\(y:x:i:a:_) -> M.insertWith (++) a [(i,(y,x))]) M.empty
            & M.map (sort >>> map snd)
        ts = last lll
            & listArray (1, f) :: UArray Int Int

    zipWith (\i fac -> cost (ts ! i) fac (M.findWithDefault [] i students)) [1..] facs
        & sort
        & take g
        & sum
        & print

cost :: Int -> [(Int,Int)] -> [(Int,Int)] -> Int
cost t f s = zipWith (\(a,b) (x,y) -> abs (a-x) + abs (b-y)) f s
    & sort
    & take t
    & sum

toTuples :: [Int] -> [(Int,Int)]
toTuples []       = []
toTuples (x:y:xs) = (x,y) : toTuples xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
