{-# LANGUAGE MultiWayIf #-}

import           Control.Applicative   (liftA2)
import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (listUArrayST, (!))
import           Data.Array.ST         (STUArray, index, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix, rangeSize)
import           Data.List             (sort)
import           Data.Maybe            (fromJust)
import           Data.STRef            (STRef, modifySTRef', newSTRef,
                                        readSTRef)

main :: IO ()
main = do
    (n,_):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    let ys = zip [0..n-1] xs    
            & edges
            & sort

    print $ runST $ do
        uf <- newUF (0,n-1)
        solve uf 0 ys

solve :: UF s Int -> Int -> [(Int,Int,Int)] -> ST s Int
solve uf ret [] = pure ret
solve uf ret ((c,u,v):xs) = do
        comps <- components uf
        conn <- connected uf u v
        if | comps == 1 -> pure ret
           | conn       -> solve uf ret xs
           | otherwise  -> merge uf u v >> solve uf (ret+c*2) xs

edges :: [(Int, (Int,Int))] -> [(Int,Int,Int)]
edges [] = []
edges ((i,(x,y)):xs) = map (\(j,(p,q)) -> (abs (x-p) + abs (y-q), i, j)) xs <> edges xs

data UF s i = UF {
    rng   :: (i,i),
    comps :: STRef    s Int,
    repr  :: STUArray s Int Int,
    sizes :: STUArray s Int Int
}

newUF :: Ix i => (i,i) -> ST s (UF s i)
newUF r = UF r <$> newSTRef n <*> listUArrayST (0,n-1) [0..] <*> newArray (0,n-1) 1
    where n = rangeSize r

find :: Ix i => UF s i -> i -> ST s Int
find uf u = find' (index (rng uf) u)
    where find' ix = do
            parent <- readArray (repr uf) ix
            if ix == parent
                then pure ix
                else do
                    grandparent <- readArray (repr uf) parent
                    writeArray (repr uf) ix grandparent
                    find' grandparent

merge :: Ix i => UF s i -> i -> i -> ST s Bool
merge uf u v = do
    p1 <- find uf u
    p2 <- find uf v
    if p1 == p2
        then pure False
        else do
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
            pure True

connected :: Ix i => UF s i -> i -> i -> ST s Bool
connected uf u v = liftA2 (==) (find uf u) (find uf v)

size :: Ix i => UF s i -> i -> ST s Int
size uf u = find uf u >>= readArray (sizes uf)

components :: UF s i -> ST s Int
components = comps >>> readSTRef

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
