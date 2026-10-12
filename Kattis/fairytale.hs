import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> solve
        >>> unlines
        >>> putStr
    )

solve :: [[Int]] -> [String]
solve [] = []
solve (_:xs:xss) = map (pred >>> (`div` 10)) xs
    & S.fromList
    & S.size
    & show
    & (:solve xss)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
