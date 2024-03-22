import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (C.words >>> tail >>> map readInt >>> minimum >>> succ >>> print)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
