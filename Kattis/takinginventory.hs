import           Control.Arrow         (second, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isDigit)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.spanEnd isDigit
            >>> second (C.readInt >>> fromJust >>> fst))
        >>> M.fromListWith (+)
        >>> M.assocs
        >>> map (\(n,a) -> n <> C.pack (show ((a+63) `div` 64)))
        >>> C.unlines
        >>> C.putStr
    )
