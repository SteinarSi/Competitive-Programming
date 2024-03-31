import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (C.lines
        >>> tail
        >>> map (C.words >>> map readInt >>> (\[a, b, c] -> a + lcm b c))
        >>> minimum
        >>> print
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
