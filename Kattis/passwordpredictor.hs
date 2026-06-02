import           Control.Arrow         ((&&&), (***), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:t:rest <- C.getContents <&> C.lines
    let (attempts,passwords) = splitAt (readInt t) rest
            & map (C.words >>> head &&& (last >>> readInt)) *** drop 1
    passwords
        & filter (\p -> all (\(a,s) -> sum (C.zipWith ((==) >>> (>>> bool 0 1)) p a) == s) attempts)
        & length
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
