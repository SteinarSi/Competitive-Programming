import           Control.Arrow         ((>>>))
import           Control.Monad         (foldM_, forM_)
import           Data.Array.Base       (MArray (getNumElements, newArray_, unsafeRead, unsafeWrite),
                                        elems, listArrayST)
import           Data.Array.ST         (runSTArray)
import qualified Data.ByteString.Char8 as C
import           Data.List             (scanl')
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> concatMap (C.words >>> map (readInt >>> fromIntegral) >>> \[x,a,b] -> [(atan (b/x),-1),(atan (a/x),1)])
        >>> sort
        >>> map snd
        >>> scanl' (+) 0
        >>> maximum
        >>> print
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

-- Borrowed from https://github.com/meooow25/haccepted/blob/master/src/Sort.hs
sort :: Ord e => [e] -> [e]
sort = sortBy compare

sortBy :: (e -> e -> Ordering) -> [e] -> [e]
sortBy cmp xs = elems $ runSTArray $ do
    a <- listArrayST (1, length xs) xs
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
{-# INLINE mergeSort #-}
