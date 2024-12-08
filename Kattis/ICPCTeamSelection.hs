import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)
import           Data.Ord              (Down (Down))

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> map readInt)
        >>> split
        >>> map (uncurry delulu >>> show >>> C.pack)
        >>> C.unlines
        >>> C.putStr
    )

delulu :: Int -> [Int] -> Int
delulu n = sortOn Down
        >>> take (2*n)
        >>> twos
        >>> sum
    where
        twos []       = []
        twos (x:y:xs) = y : twos xs

split :: [[Int]] -> [(Int,[Int])]
split []           = []
split ([n]:xs:xss) = (n, xs) : split xss

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
