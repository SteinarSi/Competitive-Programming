import           Control.Applicative   (liftA2)
import           Control.Arrow         ((>>>))
import           Control.Monad         (when, zipWithM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (elems, listUArrayST, (!))
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        runSTArray, runSTUArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap           as M
import qualified Data.List             as L
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,_]:knows:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let reps = runSTUArray $ do
            uf <- newUF n
            mapM_ (\[u,v] -> union uf u v) xs
            pure (repr uf)

        groups = runSTArray $ do
            ret <- newArray (1,n) M.empty
            zipWithM_ (\u k -> when (k/=0) (readArray ret (reps ! u) >>= (M.insertWith min k u >>> writeArray ret (reps ! u)))) [1..] knows
            pure ret

    elems groups
        & L.find (M.size >>> (>=2))
        & maybe "safe" (M.elems >>> map show >>> unwords)
        & putStrLn

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

connected :: UF s -> Int -> Int -> ST s Bool
connected uf u v = liftA2 (==) (find uf u) (find uf v)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
