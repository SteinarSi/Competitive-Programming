import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.List             (maximumBy)
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.split ',' >>> map readInt >>> zip [0..] >>> solve)
        >>> unlines
        >>> putStr
    )

solve :: [(Int,Int)] -> String
solve xs = printf "mean = %.4f median = %d mode = %d" mean median mode
  where
    count  = sum (map snd xs)
    mean   = fromIntegral (sum (map (uncurry (*)) xs)) / fromIntegral count :: Double
    median = searchMedian (1 + count `div` 2) xs
    mode   = fst (maximumBy (compare `on` snd) xs)

    searchMedian :: Int -> [(Int,Int)] -> Int
    searchMedian r ((i,x):ys)
        | x >= r    = i
        | otherwise = searchMedian (r - x) ys

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
