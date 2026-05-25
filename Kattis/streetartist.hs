import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> (head &&& (last >>> readInt)))
        >>> reverse
        >>> solve 0
        >>> reverse
        >>> C.unwords
        >>> C.putStrLn
    )

solve :: Int -> [(C.ByteString,Int)] -> [C.ByteString]
solve _ [] = []
solve p ((n,h):xs)
    | h > p = n : solve h xs
    | otherwise = solve p xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
