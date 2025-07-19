import           Control.Arrow         ((>>>))
import           Data.Array            (Array, range)
import           Data.Array.Base       (UArray, listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)
    print (solve n (listArray (1,n) xs))

solve :: Int -> UArray Int Int -> Int
solve n xs = dp ! (1,0) - xs ! 1
  where
    rng = ((1,0),(n,n))

    dp :: Array (Int,Int) Int
    dp = listArray rng (map f (range rng))

    f (i,j) | i == n    = xs ! i
            | otherwise = xs ! i + min back forward
      where
        forward = bool maxBound (dp ! (i+j+1,j+1)) (i+j+1 <= n)
        back    = bool maxBound (dp ! (i-j,j)) (i-j >= 1 && j/=0)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
