import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    _:k:xs <- C.getContents <&> (C.words >>> map readInt)

    xs
        & filter (>= (xs!!(k-1)))
        & filter (>0)
        & length
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
