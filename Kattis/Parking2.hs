import           Control.Monad         (replicateM_)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    t <- fmap readBS B.getLine
    replicateM_ t $ do
        getLine
        xs <- fmap (map readBS . BC.words) B.getLine
        print (2 * (maximum xs - minimum xs))

readBS :: B.ByteString -> Int
readBS = fst . fromJust . BC.readInt
