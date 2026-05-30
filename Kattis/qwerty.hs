import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.interact (C.lines >>> last >>> C.map interpret)

interpret :: Char -> Char
interpret ' ' = ' '
interpret  x  = layout ! x

layout :: UArray Char Char
layout = listArray ('a','z') "qwertyuiopasdfghjklzxcvbnm"
