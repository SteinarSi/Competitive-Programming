import           Control.Applicative   (liftA2)
import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (forM_, unless, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array)
import           Data.Array.Base       (listUArrayST, (!))
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        runSTArray, writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Data.STRef            (STRef, modifySTRef', newSTRef,
                                        readSTRef)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.words >>> map readInt >>> head &&& last)
        >>> parse
        >>> map (uncurry solve)
        >>> unlines
        >>> putStr
    )

solve :: Int -> Array Int [Int] -> String
solve n graph = runST $ do
    uf <- newUF n
    seen <- newArray (0,n-1) False
    search uf seen 0 [0]
    components uf <&> ((==1) >>> bool "Yes" "No")
  where
    search :: UF s -> STUArray s Int Bool -> Int -> [Int] -> ST s ()
    search uf seen p (u:us) = do
        s <- readArray seen u
        if s
            then mapM_ (merge uf u) (takeWhile (/=u) us)
            else do
                writeArray seen u True
                graph ! u
                    & filter (p/=)
                    & mapM_ (\v -> connected uf u v >>= flip unless (search uf seen u (v:u:us)))

parse :: [(Int,Int)] -> [(Int, Array Int [Int])]
parse [] = []
parse ((p,c):xs) = (p,graph) : parse zs
  where
    (ys,zs) = splitAt c xs
    graph = runSTArray $ do
        ret <- newArray (0,p-1) []
        forM_ ys $ \(x,y) -> do
            readArray ret x >>= ((y:) >>> writeArray ret x)
            readArray ret y >>= ((x:) >>> writeArray ret y)
        pure ret

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

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
