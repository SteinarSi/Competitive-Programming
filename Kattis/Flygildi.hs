import           Control.Arrow         ((&&&), (>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (permutations)
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    (n:xs,_:ys) <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt) >>> map head &&& map last)
    printf "%.6f\n" $ if all (==0) xs
        then line ys
        else bruteForce n (listArray (0,n) (0:xs)) (listArray (0,n) (0:ys))

bruteForce :: Int -> UArray Int Int -> UArray Int Int -> Double
bruteForce n xs ys = minimum [cost p | p <- permutations [1..n]]
  where
    cost :: [Int] -> Double
    cost perm = sum (zipWith dist (0:perm) (perm<>[0]))

    dist :: Int -> Int -> Double
    dist i j = sqrt (fromIntegral ((xs!i - xs!j)^2 + (ys!i - ys!j)^2))

line :: [Int] -> Double
line ys = fromIntegral (2 * (max 0 (maximum ys) + max 0 (- minimum ys)))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
