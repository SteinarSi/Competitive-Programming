import           Control.Applicative   (liftA2)
import           Control.Arrow         ((***), (>>>))
import           Control.Monad         (when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (listUArrayST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)
import           Data.STRef            (STRef, modifySTRef', newSTRef,
                                        readSTRef)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> concatMap (uncurry solve)
        >>> unlines
        >>> putStr
    )

solve :: Int -> [(Int,(Int,Int))] -> [String]
solve n edges = runST $ do
    uf <- newUF n
    (cost,tree) <- kruskall uf 0 [] edges
    d <- components uf
    if d > 1
        then pure ["Impossible"]
        else sort tree
            & map (\(u,v) -> show u <> " " <> show v)
            & (show cost:)
            & pure
  where
    kruskall :: UF s -> Int -> [(Int,Int)] -> [(Int,(Int,Int))] -> ST s (Int,[(Int,Int)])
    kruskall uf cost ret [] = pure (cost,ret)
    kruskall uf cost ret ((w,(u,v)):xs) = do
        c <- connected uf u v
        if c
            then kruskall uf cost ret xs
            else merge uf u v >> kruskall uf (cost+w) ((u,v):ret) xs

parse :: [[Int]] -> [(Int,[(Int,(Int,Int))])]
parse [] = []
parse ([n,m]:xs) = splitAt m xs
    & (map (\[u,v,w] -> (w, (min u v, max u v))) >>> sort >>> (n,))
        ***
      parse
    & uncurry (:)

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
components =  comps >>> readSTRef

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
