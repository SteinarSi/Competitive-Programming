import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.List             (sort, sortBy)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (C.words
        >>> tail
        >>> map readInt
        >>> sortBy (compare `on` negate)
        >>> shopaholic
        >>> print
    )

shopaholic :: [Int] -> Int
shopaholic (x:y:z:xs) = z + shopaholic xs
shopaholic _          = 0

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
