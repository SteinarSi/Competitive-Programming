import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:a:xs <- C.getContents <&> (C.words >>> map readInt)
    sort xs
        & map succ
        & scanl1 (+)
        & takeWhile (<=a)
        & length
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
