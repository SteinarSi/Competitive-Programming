import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> drop 1 >>> map readInt)
        >>> solve minBound
        >>> print
    )

solve :: Int -> [[Int]] -> Int
solve p [] = error "bruh"
solve p [xs] | maximum xs >= p = maximum xs
             | otherwise       = -1
solve p (xs:xss) = case filter (>=p) xs of
    [] -> -1
    ys -> solve (minimum ys) xss

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
