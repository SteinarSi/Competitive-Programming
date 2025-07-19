import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, UArray, listArray, newArray,
                                        newListArray, readArray, writeArray,
                                        (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [[n,m],hs,cs] <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    print (solve n m (listArray (1,m) cs) hs)

solve :: Int -> Int -> UArray Int Int -> [Int] -> Int
solve n m color hs = runST $ do
    rem <- newListArray (1,n) hs
    bin rem 0 m
  where
    bin :: forall s. STUArray s Int Int -> Int -> Int -> ST s Int
    bin rem lo hi | lo >= hi = pure lo
                  | otherwise = do
                    v <- valid mi 1
                    if v
                        then bin rem mi hi
                        else bin rem lo (mi-1)
      where
        mi = (lo+hi+1) `div` 2

        valid :: Int -> Int -> ST s Bool
        valid h i
            | i > h = pure True
            | otherwise = do
                let q = (h-i+1)^2
                    c = color ! i
                r <- readArray rem c
                if r < q
                    then pure False
                    else do
                        writeArray rem c (r-q)
                        v <- valid h (i+1)
                        writeArray rem c r
                        pure v

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
