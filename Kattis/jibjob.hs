import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt >>> \[x,y,h] -> ((x,y),h))
        >>> solve
        >>> map show
        >>> unlines
        >>> putStr
    )

solve :: [((Int,Int),Int)] -> [Int]
solve cranes = map jib cranes
    where
        jib :: ((Int,Int),Int) -> Int
        jib c@((x,y),h) = minimum (map job cranes)
            where
                job :: ((Int,Int),Int) -> Int
                job k@((x2,y2),h2) | c == k = h
                                   | h2 < h = maxBound
                                   | otherwise = floor (sqrt (fromIntegral ((x-x2)^2 + (y-y2)^2)))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
