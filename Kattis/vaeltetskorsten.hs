import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.readInt >>> fromJust)
        >>> filter (snd >>> (==C.pack " nej"))
        >>> maximum
        >>> fst
        >>> print
    )
