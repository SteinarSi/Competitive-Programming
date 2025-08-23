import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
        C.lines
    >>> tail
    >>> mapM_ (
            C.readInt
        >>> fromJust
        >>> fst
        >>> solve
        >>> print
        )
    )

solve :: Int -> Int
solve 0 = 0
solve n | b * 7 <= n = b
        | otherwise  = -1
    where b = case n `mod` 10 of
            7 -> 1
            4 -> 2
            1 -> 3
            8 -> 4
            5 -> 5
            2 -> 6
            9 -> 7
            6 -> 8
            3 -> 9
            0 -> 10
