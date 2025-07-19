import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    xs <- C.getContents <&> (C.words >>> drop 1 >>> map readInt)

    let m = maximum xs
        s = sum xs - m

    print (max (m+s) (2*m))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
