{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (listUArrayST)
import           Data.Array.ST         (STUArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,_]:xs <- C.getContents <&> (C.lines >>> map C.words)

    runST (listUArrayST (0, readInt n - 1) [0..] >>= \uf -> solve uf [] xs)
        & map show
        & unlines
        & putStr

solve :: STUArray s Int Int -> [Int] -> [[C.ByteString]] -> ST s [Int]
solve uf ret []               = pure (reverse ret)
solve uf ret (["DAEMON",a,b]:xs) = union uf (readInt a) (readInt b) >> solve uf ret xs
solve uf ret (["LEGAL",l]:xs) = do
    v <- find uf (readInt l)
    solve uf (v:ret) xs

find :: STUArray s Int Int -> Int -> ST s Int
find uf u = do
    parent <- readArray uf u
    if parent == u
        then pure u
        else do
            grandparent <- find uf parent
            writeArray uf u grandparent
            pure grandparent

union :: STUArray s Int Int -> Int -> Int -> ST s ()
union uf u v = do
    p1 <- find uf u
    p2 <- find uf v
    when (p1 /= p2) $ do
        if p2 < p1
            then writeArray uf p1 p2
            else writeArray uf p2 p1

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
