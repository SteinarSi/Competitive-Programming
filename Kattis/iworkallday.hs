import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.Functor          ((<&>))
import           Data.List             (minimumBy)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [_,xs,[t]] <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    print $ minimumBy (compare `on` mod t) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
