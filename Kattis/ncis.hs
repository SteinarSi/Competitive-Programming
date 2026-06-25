import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (toLower)
import           Data.List             (sortOn)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> sortOn (C.map toLower &&& id)
        >>> C.unlines
        >>> C.putStr
    )
