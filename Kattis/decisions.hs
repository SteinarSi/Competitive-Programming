import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import           Data.Bits             (shiftL)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

data BDD = Leaf Bool | Internal BDD BDD

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> map readInt
        >>> build
        >>> reduce
        >>> size
        >>> print
    )

reduce :: BDD -> BDD
reduce (Leaf x) = Leaf x
reduce (Internal l r) = case (reduce l, reduce r) of
        (Leaf x, Leaf y) | x == y -> Leaf x
        (l',r')                   -> Internal l' r'

build :: [Int] -> BDD
build (n:xs) = bu 0 0
    where
        arr = listArray (0,2^n-1) (map (1==) xs) :: UArray Int Bool

        bu :: Int -> Int -> BDD
        bu layer x | layer == n = Leaf (arr ! x)
                   | otherwise  = Internal (bu (layer+1) x) (bu (layer+1) (x + 1 `shiftL` layer))

size :: BDD -> Int
size (Leaf _)       = 1
size (Internal l r) = 1 + size l + size r

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
