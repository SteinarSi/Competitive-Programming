import           Control.Arrow         ((***), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    word:e:rest <- C.getContents <&> C.lines

    let (endings,queries) = splitAt (readInt e) rest
            & (map C.words >>> filter (any (`C.isSuffixOf` word))) *** drop 1

    queries
        & map ((\q -> any (any (`C.isSuffixOf` q)) endings) >>> bool "NO" "YES")
        & unlines
        & putStr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
