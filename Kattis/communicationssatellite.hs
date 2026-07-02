import           Control.Arrow         ((>>>))
import           Control.Monad         (when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (listUArrayST, newArray)
import           Data.Array.ST         (STUArray, readArray, runSTArray,
                                        runSTUArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (partition, sort)
import           Data.Maybe            (fromJust)
import           Data.STRef.Strict     (STRef, modifySTRef, newSTRef, readSTRef)

main :: IO ()
main = do
    [n]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let (connected,edges) = partition (fst >>> (<=0)) $ do
            (i,[x1,y1,r1]) <- zip [1..n] xs
            (j,[x2,y2,r2]) <- zip [i+1..n] (drop i xs)
            let d = sqrt (fromIntegral ((x1-x2)^2) + fromIntegral ((y1-y2)^2)) - fromIntegral r1 - fromIntegral r2
            pure (d, (i, j))
    print $ runST $ do
        uf <- newUF n
        mapM_ (snd >>> uncurry (union uf)) connected
        n' <- readSTRef (comps uf)
        sort edges
            & connect uf n' 0

connect :: UF s -> Int -> Double -> [(Double, (Int, Int))] -> ST s Double
connect uf 1 r _ = pure r
connect _ n _ [] = error "empty list"
connect uf n r ((d,(i,j)):xs) = do
    conn <- union uf i j
    if conn
        then connect uf (n-1) (d+r) xs
        else connect uf n r xs

data UF s = UF {
    comps :: STRef s Int,
    repr  :: STUArray s Int Int,
    sizes :: STUArray s Int Int
}

newUF :: Int -> ST s (UF s)
newUF n = UF <$> newSTRef n <*> listUArrayST (1,n) [1..n] <*> newArray (1,n) 1

find :: UF s -> Int -> ST s Int
find uf u = do
    parent <- readArray (repr uf) u
    if parent == u
        then pure u
        else do
            grandparent <- find uf parent
            writeArray (repr uf) u grandparent
            pure grandparent

union :: UF s -> Int -> Int -> ST s Bool
union uf u v = do
    p1 <- find uf u
    p2 <- find uf v
    when (p1 /= p2) $ do
        modifySTRef (comps uf) pred
        s1 <- readArray (sizes uf) p1
        s2 <- readArray (sizes uf) p2
        if s1 < s2
            then do
                writeArray (repr  uf) p1 p2
                writeArray (sizes uf) p2 (s1 + s2)
            else do
                writeArray (repr  uf) p2 p1
                writeArray (sizes uf) p1 (s1 + s2)
    pure (p1 /= p2)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
