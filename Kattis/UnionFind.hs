{-# LANGUAGE OverloadedStrings, BangPatterns, StrictData #-}

import qualified Data.ByteString.Char8 as C
import Control.Monad.ST (ST, runST)
import Data.Maybe (fromJust)
import Data.Bool (bool)
import Control.Monad (when)
import Data.STRef (STRef, newSTRef, modifySTRef', readSTRef)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Control.Applicative (liftA2)
import Data.Array.Base (listUArrayST)

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

readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt
  
main :: IO ()
main = do
  (!x:xs) <- C.lines <$> C.getContents
  let ![!n,!q] = map readInt $! C.words x
      queries  = map C.words xs
      results  = runST (newUF n >>= compute queries)
  mapM_ C.putStrLn results

compute :: [[C.ByteString]] -> UF s -> ST s [C.ByteString]
compute [] _ = pure []
compute (("?":a:b:_):xs) uf = do
    c <- connected uf (readInt a) (readInt b)
    (bool "no" "yes" c :) <$> compute xs uf
compute (("=":a:b:_):xs) uf = do
    merge uf (readInt a) (readInt b)
    compute xs uf
