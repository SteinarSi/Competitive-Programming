import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (Array, UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs' <- C.getContents <&> (C.words >>> map readInt)

    let xs = listArray (1,n) xs' :: UArray Int Int
        buy = listArray (1,n) (map b [1..]) :: Array Int Int
        sell = listArray (1,n) (map s [1..]) :: Array Int Int

        b :: Int -> Int
        b i | i >= n    = 0
            | otherwise = max (buy ! (i+1)) (sell ! (i+1) - xs ! i)

        s :: Int -> Int
        s i | i == n = xs ! i
            | i == n-1 = max (xs ! i) (xs ! (i+1))
            | otherwise = max (sell ! (i+1)) (xs ! i + buy ! (i+2))

    print (buy ! 1)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
