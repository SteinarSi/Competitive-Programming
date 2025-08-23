import           Control.Arrow         ((>>>))
import           Data.Bits             (popCount)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (
                readInt
            >>> prefixes
            >>> map popCount
            >>> maximum
            >>> print
        )
    )

prefixes :: Int -> [Int]
prefixes 0 = [0]
prefixes x = x : prefixes (x `div` 10)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
