import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isAlpha)
import qualified Data.Map.Strict       as M

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> filter (C.null >>> not)
        >>> map (C.words >>> last >>> C.takeWhile isAlpha >>> (,1))
        >>> M.fromListWith (+)
        >>> M.toAscList
        >>> map (\(x,y) -> C.unpack x <> " " <> show y)
        >>> unlines
        >>> putStr
    )
