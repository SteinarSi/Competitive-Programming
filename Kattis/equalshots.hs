import           Control.Arrow         ((&&&), (***), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (a,b):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    xs
        & map (uncurry (*))
        & splitAt a
        & sum *** sum
        & uncurry (==)
        & bool "different" "same"
        & putStrLn

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
