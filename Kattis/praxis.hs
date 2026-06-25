import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:_:xs <- C.getContents <&> (C.words >>> map readInt)
    putStrLn $ if maximum xs - minimum xs <= 1
        then "da komrad"
        else "ósvífinn kapítalisti"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
