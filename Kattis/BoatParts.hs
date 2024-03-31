import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (elemIndex)
import           Data.Maybe            (fromJust)
import           Data.Set              (empty, insert, size)

main :: IO ()
main = do
    p:_:parts <- C.getContents <&> C.words
    scanl (flip insert) empty parts
        & map size
        & elemIndex (readInt p)
        & maybe "paradox avoided" show
        & C.pack
        & C.putStrLn

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
