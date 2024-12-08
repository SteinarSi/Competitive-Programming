import           Control.Applicative   (liftA2)
import           Control.Arrow         ((>>>))
import           Control.Monad         (when, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (listUArrayST)
import           Data.Array.ST         (STUArray, newArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isDigit, isControl, isSpace)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as IM
import           Data.Ix               (Ix (..))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)
import           Data.STRef            (STRef, modifySTRef', newSTRef, readSTRef)

main :: IO ()
main = C.getContents >>= (
            parse
        >>> solve
        >>> putStrLn
    )

solve :: [((Int,Int),(Int,Int))] -> String
solve ps = runST $ do
        uf <- newUF ((0,0),(99,99))

        forM_ ps (uncurry (merge uf))
        ix <- concatMap (\(p,q) -> [p,q]) ps
            & mapM (\p -> find uf p <&> (,M.singleton  p 1))
            & (<&> IM.fromListWith (M.unionWith (+)))

        let figures  = IM.size ix
            polygons = IM.elems ix
                & filter (M.elems >>> all (==2))
                & length

        pure (show figures <> " " <> show polygons)

parse :: C.ByteString -> [((Int,Int),(Int,Int))]
parse = C.filter (\x -> not (isControl x || isSpace x))
        >>> C.split ';'
        >>> map (C.filter (\x -> isDigit x || x == ','))
        >>> filter (C.null >>> not)
        >>> map (C.split ','
            >>> map readInt
            >>> (\(a:b:c:d:_) -> ((a,b),(c,d))))

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