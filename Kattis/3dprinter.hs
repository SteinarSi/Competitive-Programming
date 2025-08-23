import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getContents <&> readInt

    let Just v = find ((2^) >>> (>=n)) [0..]

    print (v+1)


readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
