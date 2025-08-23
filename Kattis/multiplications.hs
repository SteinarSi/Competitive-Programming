import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (foldl')
import           Data.Maybe            (fromJust)

m :: Int -> Int
m = (`mod` 1000000007)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (readInt >>> m)
        >>> foldl' (\b x -> m (b*x)) 1
        >>> print
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
