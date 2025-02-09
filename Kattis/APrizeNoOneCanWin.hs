import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:m:xs <- C.getContents <&> (C.words >>> map readInt)

    print (solve m (sort xs))

solve :: Int -> [Int] -> Int
solve m [] = 1
solve m [x] = 1
solve m (x:y:xs) | x + y <= m = 1 + solve m (y:xs)
                 | otherwise  = 1

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
