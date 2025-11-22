import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isAlpha)

main :: IO ()
main = C.interact (
            C.lines
        >>> drop 1
        >>> C.transpose
        >>> map (C.filter isAlpha)
        >>> C.concat
    )
