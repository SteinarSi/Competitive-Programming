import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n,m):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInteger >>> head &&& last))
    print (solve m xs)

solve :: Integer -> [(Integer,Integer)] -> Integer
solve m xs = bin 1 (10^11)
    where
        bin :: Integer -> Integer -> Integer
        bin lo hi | lo >= hi        = lo
                  | invest mid >= m = bin lo mid
                  | otherwise       = bin (mid+1) hi
            where
                mid = (lo + hi) `div` 2

        invest :: Integer -> Integer
        invest d = sum (map (\(p,c) -> max 0 (p*d - c)) xs)

readInteger :: C.ByteString -> Integer
readInteger = C.readInteger >>> fromJust >>> fst
