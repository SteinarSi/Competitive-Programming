import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (toLower)

main :: IO ()
main = C.getContents >>= (
            C.filter (toLower >>> (`elem` "aeiouy"))
        >>> C.putStrLn
    )
