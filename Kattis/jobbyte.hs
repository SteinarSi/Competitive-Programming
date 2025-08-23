import           Control.Arrow         ((>>>))
import           Control.Monad         (forM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import qualified Data.IntMap           as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)
    print $ runST (newArray (1,n) False >>= solve n (listArray (1,n) xs))

solve :: Int -> UArray Int Int -> STUArray s Int Bool -> ST s Int
solve n perm seen = do
    ret <- forM [1..n] $ \u -> do
        s <- readArray seen u
        if s
            then pure 0
            else do
                let cyc = cycle u (perm ! u)
                forM_ cyc (flip (writeArray seen) True)
                pure (length cyc - 1)
    pure (sum ret)

    where
        cycle :: Int -> Int -> [Int]
        cycle start curr | start == curr = [curr]
                         | otherwise = curr : cycle start (perm ! curr)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
