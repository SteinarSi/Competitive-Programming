import           Control.Arrow         (second, (***), (>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, UArray, bounds, indices,
                                        listArray, (!))
import           Data.Array.ST         (STUArray, freeze, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.IntMap.Strict    as M
import           Data.Ix               (inRange, index)
import           Data.List             (intersperse, sortOn)
import           Data.Maybe            (fromJust, listToMaybe)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map readInt
        >>> parse
        >>> zipWith solve [1..]
        >>> concat
        >>> unlines
        >>> putStr
    )

solve :: Int -> UArray (Int,Int) Int -> [String]
solve i grid = toId 'a' M.empty (indices grid)
        & chunksOf (snd (snd rng))
        & map (intersperse ' ')
        & (("Case #" <> show i <> ":") :)
  where
    rng = bounds grid

    toId :: Char -> M.IntMap Char -> [(Int,Int)] -> [Char]
    toId _ _ []        = []
    toId curr m (x:xs) = case M.lookup (basins ! x) m of
        Nothing -> curr : toId (succ curr) (M.insert (basins ! x) curr m) xs
        Just  y -> y    : toId curr m xs

    basins :: UArray (Int,Int) Int
    basins = runST $ do
        memo <- newArray rng (-1)
        forM_ (indices grid) (basin memo)
        freeze memo

    basin :: STUArray s (Int,Int) Int -> (Int,Int) -> ST s Int
    basin memo u@(y,x) = do
        s <- readArray memo u
        if s /= -1
            then pure s
            else do
                next <- [(y-1,x),(y,x-1),(y,x+1),(y+1,x)]
                    & filter (inRange rng)
                    & filter ((grid!) >>> (< grid!u))
                    & sortOn (grid!)
                    & listToMaybe
                    & maybe (pure (index rng u)) (basin memo)
                writeArray memo u next
                pure next

parse :: [Int] -> [UArray (Int,Int) Int]
parse [] = []
parse (h:w:xs) = splitAt (h*w) xs
    & listArray ((1,1),(h,w)) *** parse
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)
