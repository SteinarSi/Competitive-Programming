{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative      (liftA2)
import           Control.Arrow            ((>>>), (***), (&&&))
import           Control.Monad            (when)
import           Control.Monad.ST         (ST, runST)
import           Data.Array.Base          (listUArrayST)
import           Data.Array.ST            (STUArray, newArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function            ((&))
import           Data.Functor             ((<&>))
import           Data.List                (sort)
import           Data.Maybe               (fromJust)
import           Data.STRef               (STRef, modifySTRef', newSTRef, readSTRef)

main :: IO ()
main = do
    ([n,e,p],rest) <- C.getContents <&> (
                C.lines 
            >>> map C.words
            >>> (head >>> map readInt) &&& tail
        )

    let (points, cables) = splitAt n rest
            & (map (map readDouble >>> (head &&& last)) >>> zip [1..]) 
                *** 
              (map (map readInt >>> (head &&& last)))

    print $ runST $ (newUF (n+1) >>= solve e points cables)

solve :: forall s. Int -> [(Int,(Double,Double))] -> [(Int,Int)] -> UF s -> ST s Double
solve e points cables uf = do
    mapM_ (merge uf 0) [1..e]
    mapM_ (uncurry (merge uf)) cables
    lines points >>= (sort >>> kruskall 0)

    where
        kruskall :: Double -> [(Double,Int,Int)] -> ST s Double
        kruskall ret [] = pure ret
        kruskall ret ((v,i,j):xs) = do
            c <- connected uf i j
            if c
                then kruskall ret xs
                else merge uf i j >> kruskall (ret+v) xs

        lines :: [(Int,(Double,Double))] -> ST s [(Double,Int,Int)]
        lines [] = pure []
        lines ((i,p):xs) = liftA2 (++) (lines' xs) (lines xs)
            where
                lines' [] = pure []
                lines' ((j,q):ys) = do
                    c <- connected uf i j
                    if c
                        then lines' ys
                        else fmap ((distance p q, i, j) :) (lines' ys)

distance :: (Double,Double) -> (Double,Double) -> Double
distance (x,y) (p,q) = sqrt ((x-p)^2 + (y-q)^2)

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
components = comps >>> readSTRef

readDouble :: C.ByteString -> Double
readDouble s | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where Just (int, r1) = C.readInt s
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
