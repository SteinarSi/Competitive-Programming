import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map readInt
        >>> sort
        >>> solve 0
        >>> map show
        >>> unwords
        >>> putStrLn
    )

solve :: Int -> [Int] -> [Int]
solve _ [] = []
solve c (x:xs) = w : solve (c+w) xs
    where w = 2*(x-c)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
