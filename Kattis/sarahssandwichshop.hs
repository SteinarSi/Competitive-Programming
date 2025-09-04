import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (toLower)

main :: IO ()
main = C.interact (
            C.lines
        >>> drop 1
        >>> map (C.map (toLower >>> (numpad !)))
        >>> C.unlines
    )

numpad :: UArray Char Char
numpad = listArray ('a','z') "22233344455566677778889999"
