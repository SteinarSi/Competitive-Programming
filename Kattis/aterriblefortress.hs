import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.interact (
            C.words
        >>> drop 1
        >>> map readInt
        >>> sum
        >>> show
        >>> C.pack
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
