import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (ap)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (ord)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map ((C.head >>> ord) &&& (C.last >>> ord))
        >>> ap (zipWith (\(a,b) (x,y) -> abs (a-x) + abs (b-y))) tail
        >>> sum
        >>> print
    )
