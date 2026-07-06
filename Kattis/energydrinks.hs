import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (delete)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:x:xs <- C.getContents <&> (C.words >>> map (readInt >>> fromIntegral))
    let first = maximum xs
    print (sum (log (first/x) / log 2 : (map (\c -> log ((x+c)/x) / log 2) (delete first xs))))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
