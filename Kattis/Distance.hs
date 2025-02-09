import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> split [] []
        >>> solve *** solve
        >>> uncurry (+)
        >>> print
    )

split :: [Int] -> [Int] -> [[Int]] -> ([Int],[Int])
split xs ys []         = (sort xs, sort ys)
split xs ys ([x,y]:zs) = split (x:xs) (y:ys) zs

solve :: [Int] -> Int
solve (x:xs) = solulu 1 x xs
    where
        n = length (x:xs)

        solulu :: Int -> Int -> [Int] -> Int
        solulu c p []     = 0
        solulu c p (y:ys) = c * (y-p) * (n-c) + solulu (c+1) y ys

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
