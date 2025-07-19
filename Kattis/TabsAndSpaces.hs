import           Control.Arrow         (second, (&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.List             (maximumBy)
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map readInt
        >>> parse
        >>> solve
        >>> putStrLn
    )

solve :: [Int] -> String
solve xs = printf "%d\n%d" tab (save tab)
  where
    tab = maximumBy (compare `on` (save &&& negate)) [1..80]
    save 1 = 0
    save t = (t-1) * sum (map (`div`t) xs)

parse :: [Int] -> [Int]
parse [] = []
parse (n:xs) = splitAt n xs
    & second parse
    & uncurry (<>)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
