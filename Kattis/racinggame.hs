import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Lazy      as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> solve 0 M.empty
        >>> unlines
        >>> putStr
    )

solve :: Int -> M.IntMap Int -> [[Int]] -> [String]
solve _ _ [] = []
solve offset times ([1,x]:xs) = solve offset (M.insertWith (+) (x-offset) 1 times) xs
solve offset times ([2,y]:xs) = solve (offset + y) times xs
solve offset times ([3,z]:xs) = show (offset + zth z (M.toAscList times)) : solve offset times xs

zth :: Int -> [(Int,Int)] -> Int
zth _ [] = error "bruh"
zth z ((x,c):xs)
    | c >= z    = x
    | otherwise = zth (z-c) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
