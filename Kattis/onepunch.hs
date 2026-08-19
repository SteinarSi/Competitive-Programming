import           Control.Arrow         ((***), (>>>))
import           Control.Monad         (forM, forM_, guard, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array, Ix (..))
import           Data.Array.Base       (UArray, assocs, freeze, listArray,
                                        listUArrayST, (!))
import           Data.Array.ST         (STArray, STUArray, newArray, readArray,
                                        writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.List             (partition)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    nmq:rest <- C.getContents <&> C.lines

    let [n,m,q] = map readInt (C.words nmq)
        rng = ((1,1),(n,m))
        (board,queries) = splitAt n rest
            & (concatMap (C.unpack >>> map (/='#')) >>> listArray @UArray rng) *** map (C.words >>> map readInt)
        (open,wall) = assocs board
            & partition snd
            & map fst *** map fst

    putStr $ unlines $ runST $ do
        uf <- newUF rng
        forM_ open $ \(y,x) -> do
            when (inRange rng (y+1,x) && board ! (y+1,x)) (union uf (y,x) (y+1,x))
            when (inRange rng (y,x+1) && board ! (y,x+1)) (union uf (y,x) (y,x+1))
        neighbours <- newArray (0, rangeSize rng - 1) S.empty :: ST s (STArray s Int S.IntSet)
        forM_ wall $ \(y,x) -> do
            ns <- [(y-1,x),(y+1,x),(y,x-1),(y,x+1)]
                    & filter (inRange rng)
                    & filter (board !)
                    & ((y,x):)
                    & mapM (find uf)
            sequence_ $ do
                u <- ns
                v <- ns
                guard (u /= v)
                pure (readArray neighbours (min u v) >>= (S.insert (max u v) >>> writeArray neighbours (min u v)))
        ns <- freeze @_ @_ @_ @_ @Array neighbours
        forM queries $ \[a,b,c,d,k] -> do
            u <- find uf (a,b)
            v <- find uf (c,d)
            u == v || k >= 1 && max u v `S.member` (ns ! min u v)
                & bool "no" "yes"
                & pure

data UF s i = UF {
    rng   :: (i,i),
    repr  :: STUArray s Int Int,
    sizes :: STUArray s Int Int
}

newUF :: Ix i => (i,i) -> ST s (UF s i)
newUF r = UF r <$> listUArrayST (0,n-1) [0..] <*> newArray (0,n-1) 1
  where
    n = rangeSize r

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

union :: Ix i => UF s i -> i -> i -> ST s ()
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

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
