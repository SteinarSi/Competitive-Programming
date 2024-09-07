import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> map readInt)
        >>> split
        >>> map (solve >>> show >>> C.pack)
        >>> C.unlines
        >>> C.putStr
    )

solve :: (Int, Int, [Int], [Int]) -> Int
solve (n,m,xs,ys) = length (filter valid xs)
    where
        sumX, sumY, avgX, avgY :: Double
        sumX = fromIntegral (sum xs)
        sumY = fromIntegral (sum ys)
        avgX = sumX / fromIntegral n
        avgY = sumY / fromIntegral m

        valid :: Int -> Bool
        valid x = avgX' > avgX && avgY' > avgY
            where avgX' = (sumX - fromIntegral x) / fromIntegral (n-1)
                  avgY' = (sumY + fromIntegral x) / fromIntegral (m+1)

split :: [[Int]] -> [(Int, Int, [Int], [Int])]
split []                    = []
split (_:(n:m:_):xs:ys:xss) = (n,m,xs,ys) : split xss

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
