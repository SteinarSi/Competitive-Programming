import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (ord)
import           Data.List             (sort)

main :: IO ()
main = C.getContents >>= (
            C.split ','
        >>> sort
        >>> map (C.unpack >>> drop 1 >>> init >>> map (ord >>> subtract (ord 'A') >>> succ) >>> sum)
        >>> zipWith (*) [1..]
        >>> sum
        >>> print
    )
