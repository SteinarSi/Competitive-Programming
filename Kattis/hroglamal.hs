import           Control.Arrow         ((&&&), (***), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isAlpha)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:rest <- C.getContents <&> C.lines
    let (dict,queries) = splitAt (readInt n) rest
            & (map (C.words >>> head &&& last) >>> M.fromList) *** drop 1

    queries
        & map (\x -> bool x (M.findWithDefault (C.cons '?' (C.snoc x '?')) x dict) (C.all isAlpha x))
        & C.unlines
        & C.putStr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
