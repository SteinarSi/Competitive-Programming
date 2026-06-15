import           Control.Arrow         ((&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt >>> head &&& last)
        >>> parse
        >>> map (uncurry solve)
        >>> unlines
        >>> putStr
    )

parse :: [(Int,Int)] -> [(Double, [(Double,Double)])]
parse [] = []
parse ((r,n):xs) = splitAt n xs
    & (map (fromIntegral *** fromIntegral) >>> lzip >>> map (\((x1,y1),(x2,y2)) -> (x2-x1, y2-y1)) >>> (fromIntegral r,)) *** parse
    & uncurry (:)

solve :: Double -> [(Double, Double)] -> String
solve r xs
    | 2 * pi * r > target = "Not possible"
    | otherwise           = show (bin 16 0.000001 1)
  where
    target = sum (map (\(x,y) -> sqrt (x^2 + y^2)) xs)

    bin :: Int -> Double -> Double -> Double
    bin i lo hi
        | i <= 0 = mi
        | otherwise = case compare (mi * target + 2 * pi * r) target of
            LT -> bin (i-1) mi hi
            GT -> bin (i-1) lo mi
            EQ -> mi
      where
        mi = (lo+hi) / 2

lzip :: [a] -> [(a,a)]
lzip xs = zip xs (last xs : init xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
