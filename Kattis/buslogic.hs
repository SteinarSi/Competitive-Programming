import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S


main :: IO ()
main = do
    mbs:xs <- C.getContents <&> C.lines
    let [m,b,s] = map readInt (C.words mbs)
    xs
        & filter ((`C.index` (m-1)) >>> (=='1'))
        & concatMap (\x -> filter (C.index x >>> (=='1')) [0..s-1])
        & S.fromList
        & S.size
        & pred
        & max 0
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
