import           Control.Arrow         ((>>>), (&&&))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> solve
        >>> mapM_ print
    )

solve :: [[Int]] -> [Int]
solve [] = []
solve ([n,m]:xs) = a 
    & map (drop 1 
        >>> ((init >>> map (count!) >>> minimum) &&& last) 
        >>> uncurry (*))
    & sum 
    & (:solve b)
    where 
        (a,c:b)  = splitAt n xs
        count :: UArray Int Int
        count = listArray (1,m) c 

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
