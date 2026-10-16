import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)

    let m = 2 * maximum xs
        diff = runST $ do
            dp <- newArray ((0,1),(m,n)) (-1)
            solve m dp 0 (zip [1..n] (sortOn negate xs))
        half = sum xs `div` 2
        left = half + (diff+1) `div` 2
        right = half - diff `div` 2

    print (left * right)

solve :: Int -> STUArray s (Int,Int) Int -> Int -> [(Int,Int)] -> ST s Int
solve m dp c [] = pure c
solve m dp c ((i,x):xs) = do
    r <- readArray dp (c,i)
    if r /= -1
        then pure r
        else do
            left <- if c+x <= m
                then solve m dp (c+x) xs
                else pure maxBound
            right <- if abs (c-x) <= m
                then solve m dp (abs (c-x)) xs
                else pure maxBound
            let ret = min left right
            writeArray dp (c,i) ret
            pure ret

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
