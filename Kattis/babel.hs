{-# LANGUAGE FlexibleContexts #-}

import           Control.Applicative   (liftA2)
import           Control.Arrow         ((***), (>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (UArray, assocs, listArray, listUArrayST,
                                        (!))
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix (..))
import           Data.List             (intersperse)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    rc:rest <- C.getContents <&> C.lines

    let [r,c] = map readInt (C.words rc)
        rng = ((0,0),(r-1,c-1))

        (grid,queries) = splitAt r rest
            & (concatMap C.unpack >>> listArray rng :: [C.ByteString] -> UArray (Int,Int) Char)
                ***
              (drop 1 >>> map (C.words >>> map readInt >>> \[y1,x1,y2,x2] -> ((y1,x1),(y2,x2))))

    putStrLn $ runST $ do
        uf <- newUF rng
        assocs grid
            & mapM_ (\((y,x),a) -> [(y+1,x),(y,x+1)]
                    & filter (inRange rng)
                    & filter ((grid!) >>> (==a))
                    & mapM_ (merge uf (y,x)))
        mapM (\(u,v) -> connected uf u v <&> bool 'N' (grid ! u)) queries <&> intersperse '\n'

data UF s i = UF {
    rng   :: (i,i),
    repr  :: STUArray s Int Int,
    sizes :: STUArray s Int Int
}

newUF :: Ix i => (i,i) -> ST s (UF s i)
newUF r = UF r <$> listUArrayST (0,n-1) [0..] <*> newArray (0,n-1) 1
    where n = rangeSize r

find :: Ix i => UF s i -> i -> ST s Int
find uf u = find' (index (rng uf) u)
  where
    find' ix = do
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

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
