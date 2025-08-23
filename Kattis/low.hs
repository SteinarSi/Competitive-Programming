import           Control.Arrow         ((>>>))
import           Data.Array.Base       (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:k:xs <- C.getContents <&> (C.words >>> map readInt)
    let m = 2*n*k
    print (solve m k (listArray (1,m) (sort xs)))

solve :: Int -> Int -> UArray Int Int -> Int
solve m k xs = bin 0 (xs ! m - xs ! 1)
  where
    bin :: Int -> Int -> Int
    bin lo hi | lo >= hi     = lo
              | feasable 0 1 = bin lo d
              | otherwise    = bin (d+1) hi
      where
        d = lo + (hi-lo) `div` 2

        feasable :: Int -> Int -> Bool
        feasable a i | i+1 > m                  = True
                     | xs ! (i+1) - xs ! i <= d = feasable (a+2*(k-1)) (i+2)
                     | otherwise                = a >= 1 && feasable (a-1) (i+1)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
