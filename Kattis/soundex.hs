import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (catMaybes)
import           System.IO

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.unpack >>> map (`M.lookup` char2code) >>> deduplicate >>> catMaybes)
        >>> unlines
        >>> putStr
    )

deduplicate :: Eq a => [a] -> [a]
deduplicate []  = []
deduplicate [x] = [x]
deduplicate (x:y:xs)
    | x == y    = deduplicate (y:xs)
    | otherwise = x : deduplicate (y:xs)

char2code :: M.Map Char Char
char2code = ["BFPV", "CGJKQSXZ", "DT", "L", "MN", "R"]
    & zipWith (\c xs -> map (,c) xs) ['1'..]
    & concat
    & M.fromList
