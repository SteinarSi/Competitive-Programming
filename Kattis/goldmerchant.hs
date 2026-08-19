import           Control.Arrow         ((***), (>>>))
import           Control.Monad         (when)
import           Control.Monad.ST      (ST)
import           Data.Array.Base       (elems, listUArrayST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        runSTArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,m]:ws:ps:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let groups = runSTArray $ do
            uf <- newUF n
            mapM_ (\[u,v] -> union uf u v) xs
            ret <- newArray (1,n) ([],[])
            sequence_ (
                zipWith3 (\i w p -> do
                    j <- find uf i
                    (jw,jp) <- readArray ret j
                    writeArray ret j (w:jw,p:jp)) [1..] ws ps
                )
            pure ret

    elems groups
        & concatMap (sort *** sort >>> uncurry (zipWith (*)))
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
