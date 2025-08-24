import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sortOn)
import           Data.Ord              (Down (Down))

main :: IO ()
main = C.interact (
            C.words
        >>> drop 1
        >>> map C.reverse
        >>> sortOn Down
        >>> C.unwords
    )
