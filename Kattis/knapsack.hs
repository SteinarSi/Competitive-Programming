import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (MArray (..))
import           Data.Array.ST         (STArray, STUArray)
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map knapsack
        >>> unlines
        >>> putStr
    )

knapsack :: (Int, Int, UArray Int Int, UArray Int Int) -> String
knapsack (n,c,value,weight) = runST $ do
    dp <- newArray (0,c) 0 :: ST s (STUArray s Int Int)
    picked <- newArray (0, n*(c+1) - 1) False :: ST s (STUArray s Int Bool)

    forM_ [0..n-1] $ \i -> do
        let w = weight ! i
            v = value ! i
        forM_ [c,c-1..w] $ \r -> do
            pick <- (v+) <$> unsafeRead dp (r - w)
            skip <- unsafeRead dp r
            when (pick > skip) $ do
                unsafeWrite dp r pick
                unsafeWrite picked (i*(c+1) + r) True

    let backtrack i r acc
            | i < 0     = pure acc
            | otherwise = do
                taken <- unsafeRead picked (i*(c+1) + r)
                if taken
                    then backtrack (i-1) (r - (weight ! i)) (i:acc)
                    else backtrack (i-1) r acc

    picked <- backtrack (n-1) c []
    pure (show (length picked) <> "\n" <> unwords (map show picked))

parse :: [[Int]] -> [(Int, Int, UArray Int Int, UArray Int Int)]
parse [] = []
parse ([c,n]:xs) = (n, c', value, weight) : parse zs
  where
    (ys,zs) = splitAt n xs
    c' = min c (sum (map last ys))
    weight = listArray (0,n-1) (map last ys)
    value = listArray (0,n-1) (map head ys)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
