import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:xs <- C.getContents <&> (C.words >>> map readInt)
    print (max (solve 0 0 xs) (solve 0 0 (reverse xs)))

solve :: Int -> Int -> [Int] -> Int
solve m c [] = 0
solve m c (x:xs)
    | x >= m    = max c (solve x 0 xs)
    | otherwise = solve m (c+m-x) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
