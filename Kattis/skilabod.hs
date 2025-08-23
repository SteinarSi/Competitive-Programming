import           Control.Arrow         (first, (***), (>>>))
import           Data.Array.Base       (UArray, listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Data.List             (sort)

main :: IO ()
main = do
    n':xs <- C.getContents <&> C.lines

    let n = readInt n'
        (dists,qs) = splitAt n xs
            & (map (C.words >>> map readInt >>> (\(x:y:_) -> x^2 + y^2)) >>> sort >>> (0:) >>> listArray (0,n))
                ***
              (drop 1 >>> map (readInt >>> (^2)))

    solve n dists qs
        & map (show >>> C.pack)
        & C.unlines
        & C.putStr

solve :: Int -> UArray Int Int -> [Int] -> [Int]
solve n dists = map (bin (0,n))
    where
        bin (lo,hi) d | lo == hi           = lo
                      | lo == hi-1         = bool hi lo (dists ! hi > d)
                      | dists ! (lo+1) > d = lo
                      | dists ! mid <= d   = bin (mid,hi) d
                      | otherwise          = bin (lo,mid-1) d
            where
                mid = (lo+hi) `div` 2

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
