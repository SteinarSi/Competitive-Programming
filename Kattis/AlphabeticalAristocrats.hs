import           Control.Arrow              ((&&&), (>>>))
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Char                  (isUpper)
import           Data.List                  (sortOn)
import           Data.Maybe                 (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> sortOn ((C.findIndex isUpper >>> fromJust) &&& id >>> uncurry C.drop)
        >>> C.unlines
        >>> C.putStr
    )
