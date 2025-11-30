{-# LANGUAGE FlexibleContexts #-}

import           Control.Applicative   (liftA2)
import           Control.Arrow         (second, (&&&), (***), (>>>))
import           Control.Monad         (filterM, void, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (UArray, assocs, listArray, listUArrayST,
                                        thaw, (!))
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix (..))
import qualified Data.List             as L
import           Data.Maybe            (fromJust)
import           Data.STRef            (STRef, modifySTRef', newSTRef,
                                        readSTRef)

main :: IO ()
main = do
    r':c':u':rest <- C.getContents <&> C.lines
    let [r,c,u] = map readInt [r',c',u']
        rng = ((1,1),(r,c))
        Just (s,_) = assocs grid
            & L.find (snd >>> (=='S'))
        (xs,qs) = splitAt r rest
            & second (map (C.words >>> map readInt >>> head &&& last))

        grid :: UArray (Int,Int) Char
        grid = listArray rng (concatMap C.unpack xs)

        ans = runST $ do
            uf <- newUF rng
            assocs grid
                & filter (snd >>> (/='.'))
                & mapM_ (\((y,x),_) -> do
                    when (y < r && grid ! (y+1,x) /= '.') (void (merge uf (y,x) (y+1,x)))
                    when (x < c && grid ! (y,x+1) /= '.') (void (merge uf (y,x) (y,x+1)))
                )
            grid' <- thaw grid
            update uf grid' s (r,c) qs
    map show ans
        & unlines
        & putStr

update :: UF s (Int,Int) -> STUArray s (Int,Int) Char -> (Int,Int) -> (Int,Int) -> [(Int,Int)] -> ST s [Int]
update uf grid s rc [] = size uf s <&> pure
update uf grid s rc ((y,x):xs) = do
    g <- size uf s
    writeArray grid (y,x) '#'
    [(y-1,x),(y+1,x),(y,x-1),(y,x+1)]
        & filter (inRange ((1,1),rc))
        & filterM (readArray grid >>> fmap (/='.'))
        >>= mapM_ (merge uf (y,x))
    (g:) <$> update uf grid s rc xs

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
