import           Control.Arrow         ((>>>))
import           Data.Array.Base       (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> init
        >>> map (readInt >>> (`solve` 1) >>> show)
        >>> unlines
        >>> putStr
    )

solve :: Int -> Int -> Int
solve n k | x < 0          = 0
          | x `mod` k == 0 = 1 + solve n (k+1)
          | otherwise      = solve n (k+1)
    where x = n - fun ! k

fun :: UArray Int Int
fun = listArray (0,length fs) (fs <> [f])
    where
        (fs,f:_) = span(<=1000000) fib

        fib :: [Int]
        fib = 0 : zipWith (+) fib [2..]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
