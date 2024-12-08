import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,m,k]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    xs
        & map (\[x,y] -> m-x + y)
        & S.fromList
        & S.size
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
