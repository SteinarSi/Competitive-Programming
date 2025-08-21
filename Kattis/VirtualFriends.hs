import           Control.Arrow         (first, (***), (>>>))
import           Control.Monad         (unless)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (MArray (..), STUArray, listUArrayST,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (foldl')
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> parse
        >>> concatMap solve
        >>> map show
        >>> unlines
        >>> putStr
    )

solve :: (Int,[(Int,Int)]) -> [Int]
solve (n,xs) = runST $ do
    uf <- newUF n
    mapM (\(u,v) -> merge uf u v >> size uf u) xs

parse :: [C.ByteString] -> [(Int,[(Int,Int)])]
parse [] = []
parse (f:xs) = splitAt (readInt f) xs
    & (map C.words
            >>> foldr (\[a,b] (m,ns) -> let (i,m')  = idOf a m
                                            (j,m'') = idOf b m'
                                        in  (m'',(i,j):ns)) (M.empty,[])
            >>> first M.size)
        *** parse
    & uncurry (:)

idOf :: C.ByteString -> M.Map C.ByteString Int -> (Int,M.Map C.ByteString Int)
idOf name m = case M.lookup name m of
    Nothing -> (M.size m, M.insert name (M.size m) m)
    Just  i -> (i, m)

data UF s = UF {
    repr  :: STUArray s Int Int,
    sizes :: STUArray s Int Int
}

newUF :: Int -> ST s (UF s)
newUF n = UF <$> listUArrayST (0,n-1) [0..] <*> newArray (0,n-1) 1

find :: UF s -> Int -> ST s Int
find uf u = do
    parent <- readArray (repr uf) u
    if parent == u
        then pure u
        else do
            grandparent <- find uf parent
            writeArray (repr uf) u grandparent
            pure grandparent

merge :: UF s -> Int -> Int -> ST s ()
merge uf u v = do
    p1 <- find uf u
    p2 <- find uf v
    unless (p1 == p2) $ do
        s1 <- size uf p1
        s2 <- size uf p2
        if s1 < s2
            then do
                writeArray (repr  uf) p1 p2
                writeArray (sizes uf) p2 (s1 + s2)
            else do
                writeArray (repr  uf) p2 p1
                writeArray (sizes uf) p1 (s1 + s2)

size :: UF s -> Int -> ST s Int
size uf u = find uf u >>= readArray (sizes uf)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
