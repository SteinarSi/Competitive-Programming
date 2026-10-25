import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt >>> \[g,p] -> show (g `div` (p+1)))
        >>> unlines
        >>> putStr
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
