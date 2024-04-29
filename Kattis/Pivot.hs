import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    xs <- C.getContents <&> (C.words >>> tail >>> map readInt)

    let l = scanl1 max xs
        r = scanr1 min xs

    zipWith3 (\x l r -> bool 0 1 (l <= x && x <= r)) xs l r
        & sum
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
