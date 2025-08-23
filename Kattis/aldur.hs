import qualified Data.ByteString.Char8 as C
import Control.Arrow ((>>>))
import Data.Maybe (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map (C.readInt >>> fromJust >>> fst)
        >>> minimum
        >>> print
    )