import           Control.Arrow ((>>>))
import           Data.Char     (ord)
import           Data.Function (on)

main :: IO ()
main = getContents >>= (
        lines
    >>> concat
    >>> zip  [ (x,y) | y <- [0..3], x <- [0..3] ]
    >>> map manhattan
    >>> sum
    >>> print
    )

manhattan :: ((Int,Int), Char) -> Int
manhattan (_,'.') = 0
manhattan ((x,y),c) = on (+) abs (x-gx) (y-gy)
    where (gy, gx) = quotRem (ord c - ord 'A') 4

