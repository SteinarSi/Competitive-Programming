import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    a:b:_:xs <- C.getContents <&> (C.words >>> map readInt)

    let (p, q) = (min a b, max a b)
        extra = xs
            & filter ((p<) &&& (<q) >>> uncurry (&&))
            & length
            & (*10)
        initial = (q-p)*4
    print (initial + extra)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
