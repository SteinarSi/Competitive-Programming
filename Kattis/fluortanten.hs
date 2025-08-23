import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (scanl')
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map readInt
        >>> filter (/=0)
        >>> (zipWith (*) [1..] >>> sum)
            &&&
            (reverse >>> scanl' (+) 0 >>> maximum)
        >>> uncurry (+)
        >>> print
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
