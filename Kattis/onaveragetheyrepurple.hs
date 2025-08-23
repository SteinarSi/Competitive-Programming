import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (filterM, forM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array, Ix, (!))
import           Data.Array.Base       (MArray (..), STUArray, bounds,
                                        readArray, writeArray)
import           Data.Array.ST         (runSTArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n,graph) <- C.getContents <&> (
                C.lines
            >>> (head >>> readInt)
                &&&
                (tail >>> map (C.words >>> map readInt >>> head &&& last))
            >>> uncurry build)

    print $ runST $ do
            seen <- newArray (1,n) False
            writeArray seen 1 True
            bfs n graph seen 0 [1]

bfs :: Int -> Array Int [Int] -> STUArray s Int Bool -> Int -> [Int] -> ST s Int
bfs n graph seen ret [] = error "bruh"
bfs n graph seen ret xs = do
        ys <- forM xs (\a -> do
            graph ! a
                & filterM (readArray seen >>> (<&> not))
                >>= mapM (\b -> writeArray seen b True >> pure b))
            <&> concat
        done <- readArray seen n
        if done
            then pure ret
            else bfs n graph seen (ret+1) ys

build :: Int -> [(Int,Int)] -> (Int, Array Int [Int])
build n xs = (,) n $ runSTArray $ do
        graph <- newArray (1,n) []
        forM_ xs $ \(a,b) -> do
            modifyArray graph a (b:)
            modifyArray graph b (a:)
        pure graph

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
