import           Control.Arrow         (second, (>>>), (&&&))
import           Control.Monad         (filterM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array, (!))
import           Data.Array.ST         (STArray, STUArray, MArray, Ix, freeze, newArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n,xs) <- C.getContents <&> (
                C.readInt
            >>> fromJust
            >>> second (
                        C.lines
                    >>> tail
                    >>> map (C.words 
                        >>> map readInt 
                        >>> head &&& last)))

    let valid = runST $ do
            arr <- newArray (0,n-1) [] :: ST s (STArray s Int [Int])
            forM_ xs (\(a,b) -> modifyArray arr a (b:) >> modifyArray arr b (a:))
            graph <- freeze arr
            left <- newArray (0,n-1) False
            right <- newArray (0,n-1) False
            solulu graph left right [0..n-1]

    putStrLn $ if valid
        then "attend here"
        else "no way"

solulu :: forall s. Array Int [Int] -> STUArray s Int Bool -> STUArray s Int Bool -> [Int] -> ST s Bool
solulu graph _ _ [] = pure True
solulu graph left right (x:xs) = do
    l <- readArray left x
    r <- readArray right x
    if l || r
        then solulu graph left right xs
        else do
            writeArray right x True 
            valid <- solulu' left right [x]
            if valid
                then solulu graph left right xs
                else pure False
    where
        solulu' :: STUArray s Int Bool -> STUArray s Int Bool -> [Int] -> ST s Bool
        solulu' _ _ [] = pure True
        solulu' l r ys = do
            zs <- concatMap (graph!) ys
                    & filterM (\y -> readArray l y >>= \a -> if a then pure False else writeArray l y True >> pure True)
            done <- mapM (readArray r) zs <&> or
            if done
                then pure False
                else solulu' r l zs

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
