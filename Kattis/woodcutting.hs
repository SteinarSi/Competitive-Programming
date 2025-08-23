import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map (uncurry solve >>> show)
        >>> unlines
        >>> putStr
    )

solve :: Int -> [Int] -> Double
solve n = sort >>> scanl1 (+) >>> sum >>> fromIntegral >>> (/ fromIntegral n)

parse :: [[Int]] -> [(Int,[Int])]
parse [] = []
parse ([n]:xss) = splitAt n xss
    & (map (drop 1 >>> sum) >>> (n,)) *** parse
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
