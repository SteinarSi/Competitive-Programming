import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> map readInt
        >>> solve
        >>> unlines
        >>> putStr
    )

solve :: [Int] -> [String]
solve []       = []
solve (x:y:xs) = show (10 * x * y) : solve xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
