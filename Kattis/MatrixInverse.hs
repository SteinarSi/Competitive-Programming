import           Control.Arrow         (second, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> map readInt
        >>> chunksOf 4
        >>> zipWith inverse [1..]
        >>> concat
        >>> putStr
    )

inverse :: Int -> [Int] -> String
inverse i xs@[a,b,c,d] = printf "Case %d:\n%d %d\n%d %d\n" i d' (-b') (-c') a'
    where
        [a',b',c',d'] = map (`div` (a*d - b*c)) xs
        det = a*d - b*c

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
        & second (chunksOf k)
        & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
