import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt >>> (\[l,r] -> 1 + r + (l+r)*(l+r+1) `div` 2) >>> show)
        >>> unlines
        >>> putStr
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
