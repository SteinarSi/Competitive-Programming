import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (toLower)
import           Data.Set              (fromList, size)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.map normalForm)
        >>> fromList
        >>> size
        >>> print
    )

normalForm :: Char -> Char
normalForm '-' = ' '
normalForm x   = toLower x
