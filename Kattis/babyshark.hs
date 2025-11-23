import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (group, maximumBy)
import qualified Data.Map.Strict       as M

main :: IO ()
main = C.interact (
            C.words
        >>> group
        >>> zipWith (\i (x:xs) -> (x, (i, length xs))) [1..]
        >>> maximumBy (\(_,(i,x)) (_,(j,y)) -> compare x y <> compare (negate i) (negate j))
        >>> fst
    )
