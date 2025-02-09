import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array)
import           Data.Array.Base       (MArray (..), freeze, getAssocs,
                                        getElems, newListArray, readArray,
                                        writeArray, (!))
import           Data.Array.ST         (STArray, STUArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,m]:ns:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    C.putStrLn $ runST $ do
            (indeg,graph) <- buildGraph n xs
            sources <- getAssocs indeg <&> (filter (snd >>> (==0)) >>> map fst)
            needs <- newListArray (0,n-1) ns
            topologicalSearch needs indeg graph sources

topologicalSearch :: STUArray s Int Int -> STUArray s Int Int -> Array Int [(Int,Int)] -> [Int] -> ST s C.ByteString
topologicalSearch needs indeg _ [] = getElems needs <&> (map show >>> unwords >>> C.pack)
topologicalSearch needs indeg graph (u:us) = do
        need <- readArray needs u
        (graph ! u)
            & filterM (\(v,w) -> do
                modifyArray needs v ((w*need)+)
                dv <- readArray indeg v
                writeArray indeg v (dv-1)
                pure (dv<=1)
            )
            >>= (map fst >>> (++us) >>> topologicalSearch needs indeg graph)

buildGraph :: Int -> [[Int]] -> ST s (STUArray s Int Int, Array Int [(Int,Int)])
buildGraph n xs = do
    indeg <- newArray (0,n-1) 0
    graph <- newArray (0,n-1) [] :: ST s (STArray s Int [(Int,Int)])

    forM_ xs $ \[u,v,w] -> do
        modifyArray indeg u succ
        modifyArray graph v ((u,w):)

    freeze graph <&> (indeg,)

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
