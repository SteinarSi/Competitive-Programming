import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (C.words >>> map readInt >>> maximum >>> print)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
