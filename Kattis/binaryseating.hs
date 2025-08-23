import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)
import           Data.Ord              (Down (Down))

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map (readInt >>> fromIntegral)
        >>> sortOn Down
        >>> solve
        >>> print
    )

solve :: [Double] -> Double
solve []     = 0
solve (x:xs) = 0.5 * x + 0.5 * solve xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
