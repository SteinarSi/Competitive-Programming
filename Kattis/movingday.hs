import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> map readInt)
        >>> (head >>> last)
                &&&
            (tail >>> map product >>> maximum)
        >>> uncurry subtract
        >>> print
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
