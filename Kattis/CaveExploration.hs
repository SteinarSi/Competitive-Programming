import           Control.Applicative   (liftA2)
import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_, unless)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array, Ix (..), (!))
import           Data.Array.Base       (listUArrayST)
import           Data.Array.ST         (MArray (..), STUArray, readArray,
                                        runSTArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,m]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let graph = runSTArray $ do
            ret <- newArray (0,n-1) []
            forM_ xs $ \[u,v] -> do
                modifyArray ret u (v:)
                modifyArray ret v (u:)
            pure ret

    print $ runST $ do
        uf <- newUF n
        stack <- newArray (0,n-1) False
        explore graph uf stack [] (-1) 0
        size uf 0

explore :: Array Int [Int] -> UF s -> STUArray s Int Bool -> [Int] -> Int -> Int -> ST s ()
explore graph uf stack ps p u = do
    seen <- readArray stack u
    if seen
        then mapM_ (merge uf u) (takeWhile (/=u) ps)
        else do
            writeArray stack u True
            forM_ (graph ! u) $ \v -> do
                c <- connected uf u v
                unless (p==v || c) (explore graph uf stack (u:ps) u v)
            writeArray stack u False

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
    unless (p1 == p2) $ do
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

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
