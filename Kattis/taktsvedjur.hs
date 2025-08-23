import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> map readInt
        >>> solulu 1 1
        >>> print
    )

solulu :: Int -> Int -> [Int] -> Int
solulu _ _ []     = 0
solulu c m (0:xs) = solulu 1 (max 1 (m `div` 2)) xs
solulu c m (x:xs) | m /= 8 && c == 2*m = 2*m*x + solulu 1 (2*m) xs
                  | otherwise          =   m*x + solulu (c+1) m xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
