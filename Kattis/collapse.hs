import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, forM_, zipWithM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (Ix, MArray, STArray, STUArray, freeze,
                                        getAssocs, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (Array, UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.List             (foldl')
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n]:xss <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    print $ runST $ do
        (graph,need,recv) <- parse n xss
        dead <- collapse graph need recv (S.singleton 1) [1]
        pure (n-dead)

collapse :: Array Int [(Int,Int)] -> UArray Int Int -> STUArray s Int Int -> S.IntSet -> [Int] -> ST s Int
collapse graph need recv dead [] = pure (S.size dead)
collapse graph need recv dead (x:xs) = do
    ys <- graph ! x
        & filter (fst >>> (`S.notMember` dead))
        & filterM (\(v,a) -> do
                r <- readArray recv v
                writeArray recv v (r-a)
                pure (r-a < need ! v)
            )
        <&> map fst
    collapse graph need recv (foldl' (flip S.insert) dead ys) (ys<>xs)

parse :: Int -> [[Int]] -> ST s (Array Int [(Int,Int)], UArray Int Int, STUArray s Int Int)
parse n xss = do
    graph <- newArray (1,n) [] :: ST s (STArray s Int [(Int,Int)])
    recv  <- newArray (1,n) 0
    let need = listArray (1,n) (map head xss)
    zipWithM_ (\u ns -> forM_ (pairs (drop 2 ns)) (\(v,a) -> do
        modifyArray recv u (a+)
        modifyArray graph v ((u,a):))) [1..] xss
    g <- freeze graph
    pure (g,need,recv)
  where
    pairs :: [Int] -> [(Int,Int)]
    pairs []       = []
    pairs [_]      = error "bruh"
    pairs (x:y:xs) = (x,y) : pairs xs

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
