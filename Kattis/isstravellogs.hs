import           Control.Arrow         ((&&&), (>>>))
import           Data.Array.ST         (newArray, readArray, runSTUArray,
                                        writeArray)
import           Data.Array.Unboxed    (elems)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))


main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map (C.head &&& C.last) >>> solve)
        >>> unlines
        >>> putStr
    )

solve :: [(Char,Char)] -> String
solve xs = runSTUArray (do
        ret <- newArray (('0','0'),('9','9')) (0::Int)
        mapM_ (\ix -> readArray ret ix >>= (succ >>> writeArray ret ix)) xs
        pure ret
    )
    & elems
    & filter (/=0)
    & map show
    & unwords
