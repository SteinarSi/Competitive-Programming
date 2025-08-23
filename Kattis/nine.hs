import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           GHC.Natural           (Natural, powModNatural)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (readInt >>> solve >>> print)
    )

solve :: Natural -> Natural
solve x = (8 * powModNatural 9 (x-1) 1000000007) `mod` 1000000007

readInt :: C.ByteString -> Natural
readInt = C.readInt >>> fromJust >>> fst >>> fromIntegral
