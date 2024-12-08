import           Control.Arrow         ((>>>))
import           Control.Monad         (when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, readArray, writeArray, newArray)
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [[n],[p],[k],cs,ts] <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let pastries = sort (flip zip [1..] cs)
        category = listArray (1,n) ts

    print $ runST $ do
            rem <- newArray (1,10^5) k
            fikashop category rem 0 p pastries

fikashop :: UArray Int Int -> STUArray s Int Int -> Int -> Int -> [(Int,Int)] -> ST s Int
fikashop category remainder ret budget [] = pure ret
fikashop category remainder ret budget ((c,i):cs) = do
        rem <- readArray remainder (category ! i)
        let purchase | rem > 0 && budget >= c = 1
                     | otherwise = 0
        when (purchase > 0) (writeArray remainder (category ! i) (rem-1))
        fikashop category remainder (ret+purchase) (budget-c*purchase) cs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
