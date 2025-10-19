import           Control.Arrow         ((>>>))
import           Control.Monad         (forM, forM_, zipWithM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        runSTUArray, writeArray)
import           Data.Array.Unboxed    (UArray, elems, listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (catMaybes, fromJust)

main :: IO ()
main = do
    n:k:xs <- C.getContents <&> (C.words >>> map readInt)
    listArray (1,n) xs
        & solve n k
        & map show
        & unwords
        & putStrLn

solve :: Int -> Int -> UArray Int Int -> [Int]
solve n k perm = elems $ runSTUArray $ do
    ret <- newArray (1,n) 0
    forM_ cycles (\cs -> iterate (map (perm!)) cs
        & (!! (k`mod`length cs))
        & zipWithM_ (writeArray ret) cs)
    pure ret
  where
    cycles = runST $ do
        seen <- newArray (1,n) False
        forM [1..n] (\s -> readArray seen s >>= bool (cycle seen s [s] (perm!s) <&> Just) (pure Nothing)) <&> catMaybes

    cycle :: STUArray s Int Bool -> Int -> [Int] -> Int -> ST s [Int]
    cycle seen s c u
        | s == u = pure c
        | otherwise = do
            writeArray seen u True
            cycle seen s (u:c) (perm ! u)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
