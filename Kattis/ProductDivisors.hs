import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (MArray (newArray), STUArray, UArray,
                                        getElems, newListArray, readArray,
                                        writeArray, (!))
import           Data.Array.ST         (runSTUArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix)
import           Data.List             (foldl')
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map readInt
        >>> solve
        >>> print
    )

solve :: [Int] -> Int
solve xs = runST $ do
    facs <- newArray (2,limit) 0
    factors facs xs
    getElems facs <&> (map succ >>> foldl' ((*) >>> (>>> (`mod` 1000000007))) 1)

factors :: forall s. STUArray s Int Int -> [Int] -> ST s ()
factors facs = mapM_ factor
    where
        factor :: Int -> ST s ()
        factor x = when (x >= 2) $ do
            let f = smallest ! x
            modifyArray facs f succ
            factor (x `div` f)

smallest :: UArray Int Int
smallest = runSTUArray $ do
    arr <- newListArray (2,limit) [2..limit]
    forM_ [2..limit] $ \k -> do
        k' <- readArray arr k
        when (k == k') (forM_ [k*k,k*(k+1)..limit] (flip (modifyArray arr) (min k)))
    pure arr

limit :: Int
limit = 10^6

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
