import           Control.Applicative   (liftA2)
import           Control.Arrow         ((>>>))
import           Control.Monad         (when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (listUArrayST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    xs <- C.getContents <&> (C.lines >>> drop 1 >>> map (C.words >>> map readInt) >>> zipWith (\i [x,y,r] -> (i,x,y,r)) [2..])
    print $ runST $ do
        uf <- newUF (length xs + 2)
        solve uf [] xs

solve :: UF s -> [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)] -> ST s Int
solve uf ps [] = error "bruh"
solve uf ps (u@(i,x,y,r):xs) = do
    when (r > x) (merge uf i 0)
    when (r > (200-x)) (merge uf i 1)
    sequence_ [merge uf i j | (j,x2,y2,r2) <- ps, (x2-x)^2 + (y2-y)^2 < (r+r2)^2]
    done <- connected uf 0 1
    if done
        then pure (length ps)
        else solve uf (u:ps) xs

data UF s = UF {
    repr  :: STUArray s Int Int,
    sizes :: STUArray s Int Int
}

newUF :: Int -> ST s (UF s)
newUF n = UF <$> listUArrayST (0,n-1) [0..] <*> newArray (0,n-1) 1

find :: UF s -> Int -> ST s Int
find uf u = do
    parent <- readArray (repr uf) u
    if parent == u
        then pure u
        else do
            grandparent <- find uf parent
            writeArray (repr uf) u grandparent
            pure grandparent

merge :: UF s -> Int -> Int -> ST s ()
merge uf u v = do
    p1 <- find uf u
    p2 <- find uf v
    when (p1 /= p2) $ do
        s1 <- size uf p1
        s2 <- size uf p2
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

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
