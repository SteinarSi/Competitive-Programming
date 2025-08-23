import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:k:xs <- C.getContents <&> (C.words >>> map readInt)

    map (1000+) xs
        & simulate 0 0 xs
        & maximum
        & (+(k-1))
        & (`div` k)
        & print

simulate :: Int -> Int -> [Int] -> [Int] -> [Int]
simulate _    curr _     []      = [curr]
simulate time curr (x:xs) ys | x <= time = simulate time (succ curr) xs ys
simulate time curr xs (y:ys) | y <= time = simulate time (pred curr) xs ys
simulate time curr (x:xs) (y:ys) = curr : simulate (min x y) curr (x:xs) (y:ys)
simulate time curr xs (y:ys) = curr : simulate y curr xs (y:ys)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
