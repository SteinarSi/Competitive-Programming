import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, forM_)
import           Control.Monad.ST      (ST)
import           Data.Array            (Array)
import           Data.Array.Base       (MArray (newArray), UArray, bounds,
                                        readArray, writeArray, (!))
import           Data.Array.ST         (STUArray, runSTArray, runSTUArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix)
import           Data.Maybe            (fromJust)
import           Data.Sequence         (Seq (..))
import qualified Data.Sequence         as Seq
import           Text.Printf           (printf)

main :: IO ()
main = do
    [n,m,k]:holes:edges <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let graph = runSTArray $ do
            ret <- newArray (1,n) []
            forM_ edges $ \(a:b:_) -> do
                modifyArray ret a (b:)
                modifyArray ret b (a:)
            pure ret
        distS = distances graph 1
        distT = distances graph n
        total = sum (map (distT!) holes)
        expected p = (k-1) * distS ! p + total - distT ! p
        numerator = map expected holes
            & (distS ! n * (k-1) :)
            & minimum
        d = gcd numerator (k-1)

    printf "%d/%d\n" (numerator `div` d) ((k-1) `div` d)

distances :: Array Int [Int] -> Int -> UArray Int Int
distances graph s = runSTUArray $ do
    dist <- newArray (bounds graph) (-1)
    writeArray dist s 0
    bfs dist (Seq.singleton s)
    pure dist
  where
    bfs :: STUArray s Int Int -> Seq Int -> ST s ()
    bfs dist Empty      = pure ()
    bfs dist (u :<| xs) = do
        du <- readArray dist u
        ys <- graph ! u
            & filterM (\v -> readArray dist v >>= \dv ->
                if dv == -1
                    then writeArray dist v (du+1) >> pure True
                    else pure False)
        bfs dist (xs <> Seq.fromList ys)

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
