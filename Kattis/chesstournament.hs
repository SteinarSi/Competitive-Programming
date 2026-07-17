{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((***), (>>>))
import           Control.Monad         (filterM, forM_, unless, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (MArray, freeze, getAssocs, getElems,
                                        listUArrayST)
import           Data.Array.ST         (STArray, STUArray, newArray, readArray,
                                        runSTUArray, writeArray)
import           Data.Array.Unboxed    (Array, Ix, UArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.List             (partition)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n',_]:xs <- C.getContents <&> (C.lines >>> map C.words)

    let n = readInt n'
        (draws,wins) = partition (\[_,s,_] -> s == "=") xs
        rep = runSTUArray $ do
            uf <- newUF n
            forM_ draws $ \[u,_,v] -> union uf (readInt u) (readInt v)
            pure (repr uf)
        possible = runST $ do
            g <- newArray (0,n-1) S.empty :: ST s (STArray s Int S.IntSet)
            indeg <- newArray (0,n-1) 0 :: ST s (STUArray s Int Int)
            forM_ wins $ \[u',_,v'] -> do
                let u = rep ! readInt u'
                    v = rep ! readInt v'
                s <- readArray g v <&> S.member u
                unless s $ do
                    modifyArray g v (S.insert u)
                    modifyArray indeg u succ
                    pure ()
            es <- getElems indeg
            graph <- freeze g <&> fmap S.toList
            getAssocs indeg >>= (filter (\(u,c) -> c == 0 && rep ! u == u) >>> map fst >>> solve graph indeg)

    putStrLn $ if possible
        then "consistent"
        else "inconsistent"

solve :: Array Int [Int] -> STUArray s Int Int -> [Int] -> ST s Bool
solve graph indeg [] = getElems indeg <&> all (==0)
solve graph indeg (u:us) = graph ! u
    & filterM (\v -> modifyArray indeg v pred <&> (==0))
    >>= ((<>us) >>> solve graph indeg)

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

size :: UF s -> Int -> ST s Int
size uf u = find uf u >>= readArray (sizes uf)

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m b
modifyArray arr ix f = do
    x <- readArray arr ix
    let y = f x
    writeArray arr ix y
    pure y

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
