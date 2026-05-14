import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Ix               (inRange)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> filter (\[u,d] -> u `mod` 8 /= 0 && not (inRange (1,10000) d))
        >>> length
        >>> print
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
