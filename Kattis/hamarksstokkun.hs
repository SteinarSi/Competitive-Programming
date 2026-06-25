import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> reverse
        >>> C.unwords
        >>> C.putStrLn
    )
