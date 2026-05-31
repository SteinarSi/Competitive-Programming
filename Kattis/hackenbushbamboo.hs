import           Control.Arrow         ((>>>))
import           Data.Bits             (xor)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map readInt
        >>> foldr xor 0
        >>> show
        >>> ('*':)
        >>> putStrLn
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
