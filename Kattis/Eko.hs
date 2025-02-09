import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:m:trees <- C.getContents <&> (C.words >>> map readInt)
    print (solve m trees)

solve :: Int -> [Int] -> Int
solve m trees = bin 0 (maximum trees)
    where
        bin :: Int -> Int -> Int
        bin lo hi | lo + 1 >= hi && cut hi = hi
                  | lo + 1 >= hi           = lo
                  | cut mid                = bin mid hi
                  | otherwise              = bin lo (mid-1)
            where
                mid = (lo + hi) `div` 2

        cut :: Int -> Bool
        cut k = map (\t -> max 0 (t - k)) trees
                & sum
                & (>=m)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
