import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:k:xs <- C.getContents <&> (C.words >>> map readInt)
    drop k xs
        & zipWith subtract xs
        & maximum
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
