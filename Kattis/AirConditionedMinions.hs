import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> map readInt >>> (\[a,b] -> (a,b)))
        >>> sortOn snd
        >>> solve 0
        >>> print
    )

solve :: Int -> [(Int,Int)] -> Int
solve _ [] = 0
solve t ((a,b):xs) | a > t = 1 + solve b xs
                   | otherwise = solve t xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
