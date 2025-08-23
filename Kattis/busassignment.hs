import qualified Data.ByteString.Char8 as C
import Control.Arrow ((>>>))
import Data.Maybe (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> concatMap (C.words >>> map readInt >>> (\(a:b:_) -> [-a, b]))
        >>> scanl (+) 0
        >>> maximum
        >>> print
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
