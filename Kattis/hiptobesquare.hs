import           Control.Arrow            ((>>>))
import           Data.Array.Unboxed       (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function            ((&))
import           Data.List                (findIndex, scanl1)
import           Data.Maybe               (fromJust)


main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (readInt
            >>> (binsearch 0 (length squares-1))
            >>> show
            >>> C.pack)
        >>> C.unlines
        >>> C.putStr
    )

binsearch :: Int -> Int -> Int -> Int
binsearch lo hi n = case compare n (memo ! mid) of
        _  | lo + 1 >= hi -> lo
        LT                -> binsearch lo mid n
        EQ                -> mid
        GT                -> binsearch mid hi n
    where
        mid = (lo + hi) `div` 2

memo :: UArray Int Int
memo = listArray (0, length squares - 1) squares

squares :: [Int]
squares = [1,3..]
        & map (\x -> x^2 - (x-2)^2) 
        & scanl1 (+)
        & takeWhile (<= 1000014128)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
