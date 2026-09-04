import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> sortOn last
        >>> solve 0
        >>> print
    )

solve :: Int -> [[Int]] -> Int
solve _ [] = 0
solve p ([s,e]:xs)
    | s < p     =     solve p xs
    | otherwise = 1 + solve e xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
