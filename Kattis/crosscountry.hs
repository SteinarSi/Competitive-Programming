import           Control.Arrow         ((***), (>>>))
import           Control.Monad         (filterM, replicateM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, getAssocs, newArray,
                                        readArray, writeArray)
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.Functor          ((<&>))
import           Data.List             (minimumBy)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:s:t:xs <- C.getContents <&> (C.words >>> map readInt)
    let graph = listArray ((0,0),(n-1,n-1)) xs
    print $ runST $ do
        done <- newArray (0,n-1) False
        dist <- newArray (0,n-1) maxBound
        writeArray dist s 0
        shortestPath n graph dist done
        readArray dist t

-- n^2, which is better than m log n in complete graphs :sunglasses:
shortestPath :: Int -> UArray (Int,Int) Int -> STUArray s Int Int -> STUArray s Int Bool -> ST s ()
shortestPath n graph dist done = replicateM_ n $ do
    (u,du) <- getAssocs dist
        >>= filterM (fst >>> readArray done >>> fmap not)
        <&> minimumBy (compare `on` snd)
    writeArray done u True
    [0..n-1]
        & map (\v -> (v, graph ! (u,v) + du))
        & filterM (readArray dist *** (<) >>> uncurry (<&>))
        >>= mapM_ (uncurry (writeArray dist))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
