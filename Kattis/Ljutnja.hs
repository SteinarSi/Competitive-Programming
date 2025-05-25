import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    m:_:xs <- C.getContents <&> (C.words >>> map readInteger)
    print (solve m xs)

solve :: Integer -> [Integer] -> Integer
solve m xs = uncurry second (first m [] xs)
  where
    s = bin 0 (maximum xs-1)

    bin :: Integer -> Integer -> Integer
    bin lo hi | lo >= hi    = lo+1
              | feasable mi = bin lo (mi-1)
              | otherwise   = bin mi hi
      where
        mi = (lo+hi+1) `div` 2

    feasable :: Integer -> Bool
    feasable d = sum [x-d | x <- xs, x > d] < m

    first :: Integer -> [Integer] -> [Integer] -> (Integer,[Integer])
    first r ys [] = (r, ys)
    first r ys (x:xs) | x > s     = first (r-(x-s)) (s:ys) xs
                      | otherwise = first r         (x:ys) xs

    second :: Integer -> [Integer] -> Integer
    second r [] = 0
    second 0 xs = sum (map (^2) xs)
    second r (x:xs) | x >= s    = (x-1)^2 + second (r-1) xs
                    | otherwise = x^2     + second r     xs

readInteger :: C.ByteString -> Integer
readInteger = C.readInteger >>> fromJust >>> fst
