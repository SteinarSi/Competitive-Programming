import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:k:xs <- C.getContents <&> (C.words >>> map readInt)
    print (solve n k xs)

solve :: Int -> Int -> [Int] -> Int
solve n k xs = bin (maximum xs) (1 + ((n+k-1)`div` k) * maximum xs)
  where
    bin :: Int -> Int -> Int
    bin lo hi | hi <= lo = hi
              | otherwise  = case attempt mi 0 (k-1) 0 psum of
                    Nothing -> bin (mi+1) hi
                    Just  m -> bin lo m
      where
        mi = (lo + hi) `div` 2

    psum = zip xs (reverse (scanl1 (+) (reverse xs)))

    attempt :: Int -> Int -> Int -> Int -> [(Int,Int)] -> Maybe Int
    attempt target m r curr [] = Just (max target m)
    attempt target m 0 curr ((_,l):_) | l <= target = Just (max l m)
                                      | otherwise   = Nothing
    attempt target m r curr ((x,l):ys) | curr+x <= target = attempt target m r (curr+x) ys
                                       | otherwise        = attempt target (max m curr) (r-1) 0 ((x,l):ys)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
