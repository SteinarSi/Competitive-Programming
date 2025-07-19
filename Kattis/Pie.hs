import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)
import           Data.Ord              (Down (Down))
import           Debug.Trace           (traceShow)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map (uncurry solve >>> printf "%.3f")
        >>> unlines
        >>> putStr
    )

solve :: Int -> [Double] -> Double
solve f pies = bin 0 999999999
  where
    bin :: Double -> Double -> Double
    bin lo hi | abs (hi - lo) <= 0.00001 = hi
              | possible mi f pies = bin mi hi
              | otherwise = bin lo mi
      where
        mi = (lo+hi) / 2

    possible :: Double -> Int -> [Double] -> Bool
    possible _ k _ | k <= 0 = True
    possible _ _ [] = False
    possible s k (x:xs) = n > 0 && possible s (k-n) xs
      where
        n = floor (x / s)

parse :: [[Int]] -> [(Int,[Double])]
parse []             = []
parse ([_,f]:xs:xss) = (1+f, map ((^2) >>> fromIntegral >>> (pi*)) (sortOn Down xs)) : parse xss

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
