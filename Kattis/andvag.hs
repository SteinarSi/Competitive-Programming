{-# LANGUAGE StrictData #-}

import           Control.Monad            (when, forM_, forM)
import           Control.Monad.ST         (ST, runST)
import           Data.Array.Base          (STUArray, listUArrayST, newArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Control.Arrow            ((>>>))
import           Data.Maybe               (fromJust)
import           Data.Functor             ((<&>))
import           Data.Bits                ((.|.), popCount)

main :: IO ()
main = do
    ([n,m,q]:ls) <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    let (channels, queries) = splitAt m ls

    let out = runST $ do
            uf <- newUF n
            forM_ channels $ \(a:b:_) -> merge uf a b

            filters <- newArray (1,n) 0 :: ST s (STUArray s Int Int)
            forM_ channels $ \(a:_:c:_) -> do
                r <- find uf a
                f <- readArray filters r
                writeArray filters r (f .|. c)

            forM queries $ \(a:b:_) -> do
                r1 <- find uf a
                r2 <- find uf b
                if r1 == r2
                    then readArray filters r1 <&> popCount
                    else pure (-1)

    forM_ out (show 
        >>> C.pack 
        >>> C.putStrLn)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

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

merge :: UF s -> Int -> Int -> ST s ()
merge uf u v = do
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
