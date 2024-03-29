{-# LANGUAGE FlexibleContexts #-}

import           Control.Applicative   (liftA2)
import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (UArray, listUArrayST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (Ix (..), listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (chr, isAlpha, ord)
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Data.STRef            (STRef, modifySTRef', newSTRef,
                                        readSTRef)

main :: IO ()
main = do
    [n, m, k] <- C.getLine <&> (C.words >>> map readInt)
    lineup <- C.getContents <&> (C.filter isAlpha >>> C.unpack >>> listArray ((1,1),(n,m)))
    print $ runST (newUF ('A', chr (ord 'A' + k - 1)) >>= solve n m lineup)

solve :: Int -> Int -> UArray (Int,Int) Char -> UF s Char -> ST s Int
solve n m lineup uf = do
    forM_ [1..m] $ \x -> forM_ [2..n] $ \y -> merge uf (lineup ! (y,x)) (lineup ! (1,x))
    components uf

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

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
