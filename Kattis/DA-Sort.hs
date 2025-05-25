import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> map readInt
        >>> drop 1
        >>> parse
        >>> map (uncurry solve)
        >>> unlines
        >>> putStr
    )

solve :: Int -> [Int] -> String
solve i xs = printf "%d %d" i (solve' (sort xs) xs)
  where
    solve' :: [Int] -> [Int] -> Int
    solve' _ [] = 0
    solve' [] _ = error "bruh"
    solve' (y:ys) (x:xs) | y == x    =     solve' ys xs
                         | otherwise = 1 + solve' (y:ys) xs

parse :: [Int] -> [(Int,[Int])]
parse [] = []
parse (i:n:xs) = splitAt n xs
    & (i,) *** parse
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
