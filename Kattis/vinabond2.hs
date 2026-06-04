{-# LANGUAGE LambdaCase #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (listUArrayST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (catMaybes, fromJust)

main :: IO ()
main = do
    [n,q]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let ans = runST $ do
            uf <- newUF n
            forM xs $ \case [1,x,y] -> union uf x y >> pure Nothing
                            [2,x]   -> size uf x <&> Just

    ans
        & catMaybes
        & map show
        & unlines
        & putStr

data UF s = UF {
    repr  :: STUArray s Int Int,
    sizes :: STUArray s Int Int
}

newUF :: Int -> ST s (UF s)
newUF n = UF <$> listUArrayST (1,n) [1..n] <*> newArray (1,n) 1

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
