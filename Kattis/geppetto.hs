import           Control.Arrow         ((&&&), (>>>))
import           Data.Array.ST         (newArray, runSTUArray, writeArray)
import           Data.Array.Unboxed    (UArray, (!))
import           Data.Bits             (popCount, shiftL, (.&.))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n,_):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    let
        ok :: UArray (Int,Int) Bool
        ok = runSTUArray $ do
            g <- newArray ((1,1),(n,n)) True
            mapM_ (\(x,y) -> writeArray g (x,y) False >> writeArray g (y,x) False) xs
            pure g

        valid :: [Int] -> Bool
        valid []     = True
        valid (y:ys) = all ((y,) >>> (ok!)) ys && valid ys

        combinations :: Int -> Int
        combinations 0 = 1
        combinations mask
            | popCount mask <= 1 || valid (filter (pred >>> shiftL 1 >>> (mask.&.) >>> (/=0)) [1..n]) = 1 + combinations (mask-1)
            | otherwise = combinations (mask-1)

    print (combinations (1 `shiftL` n -1))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
