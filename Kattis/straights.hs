import           Control.Arrow         ((***), (>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getElems, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map readInt
        >>> count
        >>> straights
        >>> print
    )

straights :: [Int] -> Int
straights [] = 0
straights xs = flush [] xs
    & map (map pred >>> straights >>> succ)
    & sum

flush :: [Int] -> [Int] -> [[Int]]
flush [] []     = []
flush ys []     = [ys]
flush [] (0:xs) = flush [] xs
flush ys (0:xs) = ys : flush [] xs
flush ys (x:xs) = flush (x:ys) xs

count :: [Int] -> [Int]
count xs = runST $ do
    cnt <- newArray (1,10000) 0 :: ST s (STUArray s Int Int)
    mapM_ (\x -> readArray cnt x >>= (succ >>> writeArray cnt x)) xs
    getElems cnt

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
