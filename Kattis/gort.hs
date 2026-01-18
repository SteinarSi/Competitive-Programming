{-# LANGUAGE QuantifiedConstraints #-}
import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (foldM_, forM_)
import           Control.Monad.ST      (ST)
import           Data.Array.Base       (IArray, MArray, UArray, elems,
                                        getNumElements, listUArrayST, newArray_,
                                        unsafeRead, unsafeWrite)
import           Data.Array.ST         (STUArray, runSTUArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n:xs,k) <- C.getContents <&> (C.lines >>> map readInt >>> init &&& last)
    sortU xs
        & drop (n`mod`k)
        & sum
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

-- Courtesy of https://github.com/meooow25/haccepted/blob/master/src/Sort.hs
sortU :: (Ord e, forall s. MArray (STUArray s) e (ST s), IArray UArray e) => [e] -> [e]
sortU = sortUBy compare

sortUBy :: (forall s. MArray (STUArray s) e (ST s), IArray UArray e) => (e -> e -> Ordering) -> [e] -> [e]
sortUBy cmp xs = elems $ runSTUArray $ do
    a <- listUArrayST (1, length xs) xs
    mergeSort a cmp
    pure a

mergeSort :: forall a e m. (MArray a e m) => a Int e -> (e -> e -> Ordering) -> m ()
mergeSort a cmp = do
    n <- getNumElements a
    b :: a Int e <- newArray_ (1, n)
    let merge l m r = foldM_ f (l, m) [l .. r-1] where
            f (i, j) k
                | i >= m = takej
                | j >= r = takei
                | otherwise = do
                    o <- cmp <$> unsafeRead a i <*> unsafeRead a j
                    if o /= GT then takei else takej
              where
                takei = (i + 1, j) <$ (unsafeWrite b k =<< unsafeRead a i)
                takej = (i, j + 1) <$ (unsafeWrite b k =<< unsafeRead a j)
    forM_ (takeWhile (<n) $ iterate (*2) 1) $ \w -> do
        forM_ [0, 2*w .. n-1] $ \i -> merge i ((i + w) `min` n) ((i + 2*w) `min` n)
        forM_ [0 .. n-1] $ \i -> unsafeRead b i >>= unsafeWrite a i
