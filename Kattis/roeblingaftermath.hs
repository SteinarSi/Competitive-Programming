import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, unless)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, assocs, inRange, listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    m':n':xs <- C.getContents <&> C.words

    let
        m = readInt m'
        n = readInt n'
        rng = ((1,1),(m, n))
        grid = listArray rng (concatMap C.unpack xs)
        Just (c,_) = find (snd >>> (=='C')) (assocs grid)
        Just rx = find ((1,) >>> (grid!) >>> (=='R')) [1..n]

        part2 = rx < n && any ((,rx+1) >>> (grid!) >>> (=='-')) [1..m]
        part1 = runST (newArray rng False >>= \seen -> dfs rng grid seen c)

    putStrLn $ if part1 && part2
        then "yes"
        else "no"

dfs :: ((Int,Int),(Int,Int)) -> UArray (Int,Int) Char -> STUArray s (Int,Int) Bool -> (Int,Int) -> ST s Bool
dfs rng grid seen (y,x)
    | grid ! (y,x) == 'R' = pure True
    | otherwise = [(y-1,x),(y-1,x+1),(y,x+1),(y+1,x+1),(y+1,x),(y+1,x-1),(y,x-1),(y-1,x-1)]
        & filter (inRange rng)
        & filter ((grid!) >>> (/='X'))
        & filterM (\v -> readArray seen v >>= \s -> unless s (writeArray seen v True) >> pure (not s))
        >>= anyM (dfs rng grid seen)

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p []     = pure False
anyM p (x:xs) = p x >>= bool (anyM p xs) (pure True)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
