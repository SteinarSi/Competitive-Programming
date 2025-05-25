import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sort, sortOn)
import           Data.Maybe            (fromJust)
import           Data.Ord              (Down (Down))

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words
            >>> drop 1
            >>> map readInt
            >>> sortOn Down
            >>> solve
            >>> show)
        >>> unlines
        >>> putStr
    )

solve :: [Int] -> Int
solve [] = 0
solve (x:y:xs) | x == y = length (dropWhile (x==) xs)
solve (_:xs) = solve xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
