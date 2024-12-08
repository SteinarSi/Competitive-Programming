import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.List             (maximumBy, transpose)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 2
        >>> map C.words
        >>> transpose
        >>> map (head &&& (tail >>> map readInt >>> sum))
        >>> maximumBy (compare `on` snd)
        >>> fst
        >>> C.putStrLn
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
