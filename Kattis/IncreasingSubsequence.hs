import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import qualified Data.IntMap.Strict    as M
import           Data.List             (minimumBy)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.words
            >>> map readInt
            >>> drop 1
            >>> reverse
            >>> solve M.empty
            >>> map show
            >>> unwords)
        >>> unlines
        >>> putStr
    )

solve :: M.IntMap (Int,[Int]) -> [Int] -> [Int]
solve memo [] = M.elems memo
    & best
    & uncurry (:)
solve memo (x:xs) = solve (M.insert x (l+1,x:s) memo) xs
  where
    prev = M.dropWhileAntitone (<=x) memo
        & M.elems
    (l,s) | null prev = (0,[])
          | otherwise = best prev

best :: [(Int, [Int])] -> (Int, [Int])
best = minimumBy (compare `on` ((fst >>> negate) &&& snd))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
