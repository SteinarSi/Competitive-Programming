import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (intToDigit)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (readInt >>> solve >>> show)
        >>> unlines
        >>> putStr
    )

solve :: Int -> Int
solve n
    | length xs < 10-x && n <= suggestion = suggestion
    | x < 9 = solve (format (x+1 : replicate (length xs) 0))
    | otherwise = solve (format (1 : replicate (length xs + 1) 0))
  where
    suggestion = format (x : take (length xs) [x+1..])
    x:xs = reverse (digits n)

format :: [Int] -> Int
format xs = sum (zipWith (*) (reverse xs) (map (10^) [0..]))

digits :: Int -> [Int]
digits 0 = []
digits x = r : digits q
  where
    (q,r) = quotRem x 10

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
