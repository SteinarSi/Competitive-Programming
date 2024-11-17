import Data.List (elemIndex, transpose, intercalate)
import Control.Arrow ((>>>), second, (***))
import Data.Function ((&))
import Data.Maybe (isJust, fromJust)

main :: IO ()
main = getContents >>= (
            lines
        >>> map split
        >>> transpose
        >>> map (`elemIndex` ascii)
        >>> span isJust
        >>> second tail
        >>> (interpret *** interpret)
        >>> uncurry (+)
        >>> digits
        >>> map (ascii !!)
        >>> transpose
        >>> map (intercalate ".")
        >>> mapM_ putStrLn
    )

interpret :: [Maybe Int] -> Int
interpret = map fromJust
    >>> reverse
    >>> zipWith (*) (map (10^) [0..])
    >>> sum

split :: String -> [String]
split ""  = []
split [_] = []
split xs  = splitAt 5 xs
    & second (drop 1 >>> split)
    & uncurry (:)

digits :: Int -> [Int]
digits 0 = [0]
digits x = deets x 
        & reverse
    where
        deets 0 = []
        deets x = x `mod` 10 : deets (x `div` 10)

ascii :: [[String]]
ascii = [
   ["xxxxx",
    "x...x",
    "x...x",
    "x...x",
    "x...x",
    "x...x",
    "xxxxx"],

   ["....x",
    "....x",
    "....x",
    "....x",
    "....x",
    "....x",
    "....x"],

   ["xxxxx",
    "....x",
    "....x",
    "xxxxx",
    "x....",
    "x....",
    "xxxxx"],

   ["xxxxx",
    "....x",
    "....x",
    "xxxxx",
    "....x",
    "....x",
    "xxxxx"],

   ["x...x",
    "x...x",
    "x...x",
    "xxxxx",
    "....x",
    "....x",
    "....x"],

   ["xxxxx",
    "x....",
    "x....",
    "xxxxx",
    "....x",
    "....x",
    "xxxxx"],

   ["xxxxx",
    "x....",
    "x....",
    "xxxxx",
    "x...x",
    "x...x",
    "xxxxx"],

   ["xxxxx",
    "....x",
    "....x",
    "....x",
    "....x",
    "....x",
    "....x"],

   ["xxxxx",
    "x...x",
    "x...x",
    "xxxxx",
    "x...x",
    "x...x",
    "xxxxx"],

   ["xxxxx",
    "x...x",
    "x...x",
    "xxxxx",
    "....x",
    "....x",
    "xxxxx"]]
