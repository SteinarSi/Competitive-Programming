import           Control.Arrow         ((>>>))
import           Data.Array.Base       (UArray, listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,m,k]:xs:ys:_ <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let with    = count n (map (+k) xs) ys
        without = count n xs ys
        boost i = without ! n - (without ! min n (i+m-1) - without ! (i-1)) + (with ! min n (i+m-1) - with ! (i-1))

    find (boost >>> (>0)) [1..n]
        & maybe "Neibb" (pred >>> show)
        & putStrLn

count :: Int -> [Int] -> [Int] -> UArray Int Int
count n xs ys = zipWith (\x y -> case compare x y of
        LT -> -1
        EQ -> 0
        GT -> 1
    ) xs ys
    & scanl (+) 0
    & listArray (0,n)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
