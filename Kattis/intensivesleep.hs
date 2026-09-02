import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,c,k]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    print (solve n 0 (0 : map last xs) (drop k (map head xs)))

solve :: Int -> Int -> [Int] -> [Int] -> Int
solve n !r (b:bs) []     = max r (n-b)
solve n !r (b:bs) (a:as) = solve n (max r (a-b)) bs as

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
