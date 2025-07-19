import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (foldl1')
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> map readInteger >>> foldl1' lcm >>> show)
        >>> unlines
        >>> putStr
    )

readInteger :: C.ByteString -> Integer
readInteger = C.readInteger >>> fromJust >>> fst
