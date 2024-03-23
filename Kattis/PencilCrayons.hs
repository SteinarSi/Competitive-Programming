import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Set              (fromList, size)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> (\xs -> length xs - size (fromList xs)))
        >>> sum
        >>> print
    )
