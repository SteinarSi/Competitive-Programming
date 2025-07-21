import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    (n,_):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& (tail >>> S.fromList)))
    let ops = M.fromListWith (<>) xs
        unique = S.size (S.fromList (M.elems ops))

    print $ if n == M.size ops
        then unique
        else 1 + unique

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
