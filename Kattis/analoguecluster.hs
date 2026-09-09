import           Control.Arrow         ((>>>))
import           Control.Monad         (when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (UArray, getElems, listArray,
                                        listUArrayST, (!))
import           Data.Array.ST         (STArray, STUArray, newArray, readArray,
                                        runSTUArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,c]:xs:ys <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    let
        intakes :: UArray Int Int
        intakes = listArray (1,n) xs

        reps = runSTUArray $ do
            uf <- newUF n
            mapM_ (\[a,b] -> union uf a b) ys
            pure (repr uf)

        counts = runST $ do
            ret <- newArray (1,n) M.empty :: ST s (STArray s Int (M.IntMap Int))
            mapM_ (\u -> readArray ret (reps ! u) >>= (M.insertWith (+) (intakes ! u) 1 >>> writeArray ret (reps ! u))) [1..n]
            getElems ret

    counts
        & map M.elems
        & filter (null >>> not)
        & map (\m -> sum m - maximum m)
        & sum
        & print

data UF s = UF {
    repr  :: STUArray s Int Int,
    sizes :: STUArray s Int Int
}

newUF :: Int -> ST s (UF s)
newUF n = UF <$> listUArrayST (1,n) [1..] <*> newArray (1,n) 1

find :: UF s -> Int -> ST s Int
find uf u = do
    parent <- readArray (repr uf) u
    if parent == u
        then pure u
        else do
            grandparent <- find uf parent
            writeArray (repr uf) u grandparent
            pure grandparent

union :: UF s -> Int -> Int -> ST s ()
union uf u v = do
    p1 <- find uf u
    p2 <- find uf v
    when (p1 /= p2) $ do
        s1 <- readArray (sizes uf) p1
        s2 <- readArray (sizes uf) p2
        if s1 < s2
            then do
                writeArray (repr  uf) p1 p2
                writeArray (sizes uf) p2 (s1 + s2)
            else do
                writeArray (repr  uf) p2 p1
                writeArray (sizes uf) p1 (s1 + s2)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
