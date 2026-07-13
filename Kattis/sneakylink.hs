import           Control.Arrow         (second, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (partition, sort)
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
solve []             = []
solve ([n,c]:xs:xss) = partition (>=0) xs
    & second (sort >>> map negate >>> zipWith subtract [c,2*c..] >>> filter (>=0))
    & uncurry (<>)
    & sum
    & show
    & (:solve xss)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
