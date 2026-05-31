import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (elemIndex, scanl')
import           Data.Maybe            (fromJust, fromMaybe)

main :: IO ()
main = do
    r:s:_:xs <- C.getContents <&> (C.words >>> map readInt)
    scanl' ((+) >>> (>>>(`mod` s))) 0 xs
        & elemIndex r
        & fromMaybe (-1)
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
