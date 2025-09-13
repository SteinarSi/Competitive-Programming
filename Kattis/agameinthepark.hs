import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, elems, listArray, (!))
import           Data.Bits             (shiftL, (.&.))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (isInfixOf)
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)
    let grid = listArray ((1,1),(4,n)) xs
    print (solve n grid 1 (listArray (0,10) (repeat 0)))

solve :: Int -> UArray (Int,Int) Int -> Int -> UArray Int Int -> Int
solve n grid i prev
    | i > n = maximum (elems prev)
    | otherwise = masks4masks
        & map (\(ns,ix) -> sum (map ((,i) >>> (grid!)) ix) + maximum (0:map (prev!) ns))
        & listArray (0,10)
        & solve n grid (i+1)

masks4masks :: [([Int],[Int])]
masks4masks = map mask [0..10]
  where
    mask m | "11" `isInfixOf` printf "%b" m = ([],[])
           | otherwise = (filter ((m.&.) >>> (==0)) [0..10], filter (pred >>> shiftL 1 >>> (.&.m) >>> (/=0)) [1..4])

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
