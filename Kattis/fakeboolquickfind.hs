import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getElems, listUArrayST,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,_]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    putStrLn $ runST $ do
        uf <- listUArrayST (0,n-1) [0..]
        mapM_ (\[x,y] -> merge uf x y) xs
        mapM (find uf) [0..n-1] <&> (map show >>> unwords)

find :: STUArray s Int Int -> Int -> ST s Int
find uf u = do
    parent <- readArray uf u
    if parent == u
        then pure u
        else do
            grandparent <- find uf parent
            writeArray uf u grandparent
            pure grandparent

merge :: STUArray s Int Int -> Int -> Int -> ST s ()
merge uf u v = do
    p1 <- find uf u
    p2 <- find uf v
    case compare p1 p2 of
        GT -> writeArray uf p1 p2
        EQ -> pure ()
        LT -> writeArray uf p2 p1

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
