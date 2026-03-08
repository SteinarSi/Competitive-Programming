import           Control.Arrow    ((>>>))
import           Control.Monad    (filterM)
import           Control.Monad.ST (ST, runST)
import           Data.Array       (Array, listArray, (!))
import           Data.Array.ST    (STUArray, newArray, readArray, writeArray)
import           Data.Foldable    (foldlM)
import           Data.Function    ((&))
import           Data.Functor     ((<&>))
import           Data.List        (sortOn)

main :: IO ()
main = do
    [n]:xss <- getContents <&> (lines >>> map (words >>> map read))
    let graph = listArray (0,n-1) xss
        x:xs = sortOn ((graph!) >>> length >>> negate) [0..n-1]
    print $ if all (length >>> (==n-1)) xss
        then n
        else runST $ do
            possible <- newArray ((0,0),(n-1,n-1)) True
            mapM_ (\y -> writeArray possible (y,0) False) (graph ! x)
            color n graph possible n 1 xs

color :: forall s. Int -> Array Int [Int] -> STUArray s (Int,Int) Bool -> Int -> Int -> [Int] -> ST s Int
color _ _ _ best k [] = pure (min best k)
color n graph possible best k (x:xs)
    | k > best = pure best
    | otherwise = [0..min (n-2) k]
        & filterM (\c -> readArray possible (x,c))
        >>= foldlM branch best
  where
    branch :: Int -> Int -> ST s Int
    branch b c = do
        revert <- filterM (\y -> readArray possible (y,c)) (graph ! x)
        mapM_ (\y -> writeArray possible (y,c) False) revert
        let k' | c >= k    = k+1
               | otherwise = k
        ret <- color n graph possible b k' xs
        mapM_ (\y -> writeArray possible (y,c) True) revert
        pure ret
