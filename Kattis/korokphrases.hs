import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import qualified Data.Set              as S

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> S.fromList
        >>> S.size
        >>> print
    )
