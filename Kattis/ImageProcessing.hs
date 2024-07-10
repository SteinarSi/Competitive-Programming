import           Control.Arrow            ((>>>), (***))
import           Data.Array.Unboxed       (UArray, listArray, (!), indices)
import qualified Data.ByteString.Char8 as C
import           Data.Function            ((&))
import           Data.Functor             ((<&>))
import           Data.Maybe               (fromJust)

main :: IO ()
main = do
    h:w:n:m:xs <- C.getContents <&> (C.lines >>> concatMap (C.words >>> map readInt))

    splitAt (h*w) xs
        & listArray ((1,1),(h,w)) 
            *** 
          (reverse >>> listArray ((1,1),(n,m)))
        & solve (h,w,n,m)
        & C.putStr

solve :: (Int, Int, Int, Int) -> (UArray (Int,Int) Int, UArray (Int,Int) Int) -> C.ByteString
solve (h,w,n,m) (image,kernel) = map (\y -> map (cell y) [0..w-m] & C.unwords) [0..h-n] & C.unlines
    where
        cell :: Int -> Int -> C.ByteString
        cell y x = [ image ! (y+i,x+j) * kernel ! (i,j) | (i,j) <- indices kernel ]
                & sum
                & show
                & C.pack

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
