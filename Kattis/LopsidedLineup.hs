import           Control.Arrow         ((&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words
            >>> map (C.readInt
                >>> fromJust
                >>> fst)
            >>> sum)
        >>> (head >>> (`div` 2))
                &&&
            (drop 1 >>> sort)
        >>> uncurry splitAt
        >>> sum *** sum
        >>> uncurry subtract
        >>> (`div` 2)
        >>> print
    )
