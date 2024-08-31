import           Control.Arrow         (first, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isDigit, ord)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust, fromMaybe)

main :: IO ()
main = do
    xs:n':ys:_ <- C.getContents <&> C.words

    let (x,y) = (parse M.empty xs, parse M.empty ys)
        n = C.readInt n'
            & fromJust
            & fst

    M.assocs y
        & map (first (flip (M.findWithDefault 0) x >>> (*n))
            >>> uncurry div)
        & minimum
        & print

parse :: M.IntMap Int -> C.ByteString -> M.IntMap Int
parse m xs
    | C.null xs = m
    | otherwise = C.tail xs
        & C.readInt
        & fromMaybe (1,C.tail xs)
        & first (flip (M.insertWith (+) (ord (C.head xs))) m)
        & uncurry parse
