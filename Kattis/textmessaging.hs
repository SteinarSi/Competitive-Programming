import           Control.Arrow         (second, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map solve
        >>> zipWith (printf "Case #%d: %d") [(1::Int)..]
        >>> unlines
        >>> putStr
    )

solve :: (Int,Int,Int,[Int]) -> Int
solve (p,k,l,xs) = sortOn negate xs
    & chunksOf k
    & zipWith (\s -> map (s*) >>> sum) [1..]
    & sum

parse :: [[Int]] -> [(Int,Int,Int,[Int])]
parse []               = []
parse ([p,k,l]:xs:xss) = (p,k,l,xs) : parse xss

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
