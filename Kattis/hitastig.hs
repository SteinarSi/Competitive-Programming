import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (hi,lo) <- C.getContents <&> (
            C.words
        >>> tail
        >>> map readInt
        >>> maximum &&& minimum)
    putStrLn (show hi <> " " <> show lo)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
