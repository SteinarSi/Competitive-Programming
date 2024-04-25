import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, forM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getAssocs, newArray,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.Functor          ((<&>))
import           Data.List             (maximumBy)
import           Data.Maybe            (fromJust)
import qualified Data.Vector.Mutable   as MV

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    horror <- C.getLine <&> (C.words >>> map readInt)
    sims <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> (\(a:b:_) -> (a,b))))

    let ans = runST $ do
            graph <- MV.replicate n [] :: ST s (MV.STVector s [Int])
            forM_ sims $ \(a,b) -> do
                    MV.modify graph (a:) b
                    MV.modify graph (b:) a
            hi <- newArray (0,n-1) 99999999999
            forM_ horror $ \i -> writeArray hi i 0
            bfs 1 hi graph horror

    print ans

bfs :: Int -> STUArray s Int Int -> MV.STVector s [Int] -> [Int] -> ST s Int
bfs d hi graph [] = getAssocs hi <&> (maximumBy (compare `on` (\(a,b)->(b,-a))) >>> fst)
bfs d hi graph xs = do
    nx <- forM xs $ \u -> do
        neigh <- MV.read graph u
        next <- filterM (readArray hi >>> fmap (>d)) neigh
        forM_ next (flip (writeArray hi) d)
        pure next
    bfs (d+1) hi graph (concat nx)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

