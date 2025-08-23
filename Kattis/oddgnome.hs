import Control.Monad (replicateM_)
import Data.List (elemIndices)

main :: IO ()
main = getLine >>= \n -> replicateM_ (read n) (fmap (map read . tail . words) getLine >>= \g -> print $ (2+) $ head $ elemIndices False $ zipWith (\a b -> a+1 == b) g (tail g))