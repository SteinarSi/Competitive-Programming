import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (delete)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    xs <- C.getContents <&> (C.words >>> drop 1 >>> map readInt)
    print (maximum (delete (maximum xs) xs))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
