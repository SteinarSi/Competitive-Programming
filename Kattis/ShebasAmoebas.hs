{-# LANGUAGE StrictData #-}

import           Control.Applicative   (liftA2)
import           Control.Category      ((>>>))
import           Control.Monad         (forM_, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (UArray, bounds, indices, listArray,
                                        listUArrayST, (!))
import           Data.Array.ST         (STUArray, inRange, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)
import           Data.STRef            (STRef, modifySTRef', newSTRef,
                                        readSTRef)

main :: IO ()
main = do
    [n, m] <- C.getLine <&> (C.words >>> map (C.readInt >>> fromJust >>> fst))
    C.getContents >>= (
                C.filter (`elem` ".#")
            >>> C.unpack
            >>> map ('#'==)
            >>> listArray ((0,0),(n-1,m-1))
            >>> solve n m
            >>> print
        )

solve :: Int -> Int -> UArray (Int,Int) Bool -> Int
solve n m dish = runST $ do
    uf <- newUF (n*m)
    let cells = filter (dish!) (indices dish)
    forM_ cells $ \(x,y) -> do
        [ (x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1]]
            & filter (/=(x,y))
            & filter (inRange (bounds dish))
            & filter (dish!)
            & mapM_ (index >>> merge uf (index (x,y)))
    mapM (index >>> find uf) cells <&> (S.fromList >>> S.size)
    where index (w,h) = h*n + w

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

merge :: UF s -> Int -> Int -> ST s ()
merge uf u v = do
    p1 <- find uf u
    p2 <- find uf v
    when (p1 /= p2) $ do
        modifySTRef' (comps uf) pred
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

components :: UF s -> ST s Int
components = readSTRef . comps
