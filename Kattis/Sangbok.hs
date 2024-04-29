import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    t:_:xs <- C.getContents <&> (C.words >>> map readInt)

    sort xs
        & scanl (+) 0
        & takeWhile (<=t * 60)
        & last
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
