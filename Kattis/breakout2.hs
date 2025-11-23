import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (partition)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:x:_:xs <- C.getContents <&> (C.words >>> map readInt)
    let (l,r) = partition (<x) xs
    print (min (length l) (length r))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
