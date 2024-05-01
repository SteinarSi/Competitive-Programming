import           Control.Arrow         ((>>>))
import           Data.Array            (Array, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (readInt >>> (fib !) >>> print)
    )

fib :: Array Int Int
fib = arr
    where arr = listArray (1,10000) $ 2 : 3 : map (\i -> (arr ! (i-1) + arr ! (i-2)) `mod` 1000000007) [3..]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
