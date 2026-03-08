import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.Functor          ((<&>))
import           Data.List             (minimumBy)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n,s):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))
    let (jx,jt) = minimumBy (compare `on` \(x,t) -> x + t + s - 8 * x) xs
    xs
        & map (\(x,t) -> 8 * x + t + abs (jx - x) + jt + s - 8 * jx)
        & (s:)
        & minimum
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
