import           Control.Applicative (liftA2)
import           Control.Arrow       ((>>>))
import           Control.Monad       (when)
import           Control.Monad.ST    (ST)
import           Data.Array.Base     (listUArrayST)
import           Data.Array.ST       (STUArray, newArray, readArray, writeArray)
import           Data.STRef          (STRef, modifySTRef', newSTRef, readSTRef)

data UF s = UF {
    comps :: STRef    s Int,
    repr  :: STUArray s Int Int,
    sizes :: STUArray s Int Int
}

newUF :: Int -> ST s (UF s)
newUF n = UF <$> newSTRef n <*> listUArrayST (0,n-1) [0..] <*> newArray (0,n-1) 1

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
        modifySTRef' (comps uf) pred
        s1 <- readArray (sizes uf) p1
        s2 <- readArray (sizes uf) p2
        if s1 < s2
            then do
                writeArray (repr  uf) p1 p2
                writeArray (sizes uf) p2 (s1 + s2)
            else do
                writeArray (repr  uf) p2 p1
                writeArray (sizes uf) p1 (s1 + s2)

connected :: UF s -> Int -> Int -> ST s Bool
connected uf u v = liftA2 (==) (find uf u) (find uf v)

size :: UF s -> Int -> ST s Int
size uf u = find uf u >>= readArray (sizes uf)

components :: UF s -> ST s Int
components = comps >>> readSTRef
