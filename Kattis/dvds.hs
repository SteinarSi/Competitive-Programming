import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> split
        >>> map (order 1 >>> show >>> C.pack)
        >>> C.unlines
        >>> C.putStr
    )

split :: [C.ByteString] -> [[Int]]
split []         = []
split (_:xs:xss) = map (C.readInt >>> fromJust >>> fst) (C.words xs) : split xss

order :: Int -> [Int] -> Int
order _ [] = 0
order i (x:xs) | i == x    = order (i+1) xs
               | otherwise = 1 + order i xs
