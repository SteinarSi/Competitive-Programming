import           Control.Arrow         ((&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> map head
                &&&
            map last
        >>> ((maximum >>> succ) &&& (minimum >>> pred) >>> uncurry (-))
                ***
            ((maximum >>> succ) &&& (minimum >>> pred) >>> uncurry (-))
        >>> uncurry (+)
        >>> (*2)
        >>> print
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
