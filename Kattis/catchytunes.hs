import           Control.Arrow         ((&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (partition)
import qualified Data.Map.Strict       as M

main :: IO ()
main = C.interact (
            C.lines
        >>> drop 1
        >>> map (C.words >>> head &&& (last >>> pure))
        >>> M.fromListWith (++)
        >>> M.elems
        >>> partition (length >>> (>1))
        >>> concat *** concat
        >>> uncurry merge
        >>> C.unlines
        )

merge :: [C.ByteString] -> [C.ByteString] -> [C.ByteString]
merge [] ys         = ys
merge [x] []        = [x]
merge _ []          = error "bruh"
merge (x:xs) (y:ys) = x : y : merge xs ys
