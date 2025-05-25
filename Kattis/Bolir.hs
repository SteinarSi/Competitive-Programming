import           Control.Arrow         (first, (&&&), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap           as M
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> (init >>> map (head &&& last) >>> sort)
                &&&
            (last >>> sort)
        >>> uncurry (solve M.empty)
        >>> bool "Neibb" "Jebb"
        >>> putStrLn
    )

solve :: M.IntMap Int -> [(Int,Int)] -> [Int] -> Bool
solve _ _ [] = True
solve avail prefs (s:ss) = case next of
    Nothing         -> False
    Just (x,avail') -> x >= s && solve avail' prefs' ss
  where
    (next,prefs') = span (fst >>> (<=s)) prefs
        & first (map (snd >>> (,1))
            >>> M.fromListWith (+)
            >>> M.unionWith (+) avail
            >>> pop)

pop :: M.IntMap Int -> Maybe (Int, M.IntMap Int)
pop m = case M.minViewWithKey m of
    Nothing         -> Nothing
    Just ((k,1),m') -> Just (k,m')
    Just ((k,0),m') -> pop m'
    Just ((k,c),m') -> Just (k, M.insert k (c-1) m')

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
