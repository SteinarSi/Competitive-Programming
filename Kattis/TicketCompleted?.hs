import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, listUArrayST, newArray,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,_]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let sz = runST $ do
            uf <- newUF n
            mapM_ (\[x,y] -> merge uf x y) xs
            new <- newArray (1,n) True :: ST s (STUArray s Int Bool)
            mapM (find uf) [1..n]
                >>= filterM (\p -> readArray new p >>= \r -> when r (writeArray new p False) >> pure r)
                >>= mapM (size uf)
        nom = sum $ map (\s -> s*(s-1) `div` 2) sz
        den = n * (n-1) `div` 2

    print (fromIntegral nom / fromIntegral den)

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

size :: UF s -> Int -> ST s Int
size uf u = find uf u >>= readArray (sizes uf)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
