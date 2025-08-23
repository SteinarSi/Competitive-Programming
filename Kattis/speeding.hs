import           Control.Arrow         ((>>>))
import           Control.Monad         (ap)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> map (C.readInt >>> fromJust >>> fst) >>> (\(a:b:_) -> (a, b)))
        >>> ap (zipWith speed) tail
        >>> maximum
        >>> print
    )

speed :: (Int,Int) -> (Int,Int) -> Int
speed (t1, p1) (t2, p2) = (p2-p1) `div` (t2-t1)
