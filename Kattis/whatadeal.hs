import           Control.Arrow         (second, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)
import           Data.Ratio            ((%))

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.readInt
            >>> fromJust
            >>> second (C.tail
                >>> C.readInt
                >>> fromJust
                >>> fst))
        >>> sortOn (\(g,r) -> (g % r, -g))
        >>> map (\(g,r) -> C.pack (show g <> " " <> show r))
        >>> C.unlines
        >>> C.putStr
    )
