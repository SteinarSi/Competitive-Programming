import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort, sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [w,_]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    xs
        & map (\(v:vs) -> convince v (sortOn negate vs))
        & sort
        & take (1 + w `div` 2)
        & sum
        & print

convince :: Int -> [Int] -> Int
convince secured [] = 0
convince secured (x:xs)
    | secured > x = 0
    | x > 1       = 1 + convince (secured+1) (insert (x-1) xs)
    | otherwise   = 1 + convince (secured+1) xs

insert :: Int -> [Int] -> [Int]
insert p [] = [p]
insert p (x:xs)
    | p < x     = x : insert p xs
    | otherwise = p : x : xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
