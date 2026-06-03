import           Control.Arrow ((&&&), (***), (>>>))
import           Control.Monad (ap)
import           Data.Bool     (bool)
import           Data.Function ((&))
import           Data.List     (foldl1')
import           Text.Printf   (printf)

main :: IO ()
main = getContents >>= (
            lines
        >>> drop 1
        >>> map (words >>> map read >>> \[a,b,c,d,l,r] -> solulu a b c d l r)
        >>> unlines
        >>> putStr
    )

solulu :: Double -> Double -> Double -> Double -> Double -> Double -> String
solulu a b c d l r = format $ case intersections of
    [] -> section l r
    xs -> foldl1' (++++) (zipWith section (l:xs) (xs<>[r]))
  where
    intersections = merge (bool [] (family f1) (a+c /= 0)) (bool [] (family f2) (a-c /= 0))

    f1 n = (pi*(2*n+1) - b - d) / (a+c)
    f2 n = (2*n*pi - b + d) / (a-c)

    diff x = sin (a * x + b) - sin (c * x + d)

    section :: Double -> Double -> (Double,Double,Double,Double)
    section x1 x2
        | diff ((x1 + x2) / 2) > 0 = (2 * (x2 - x1) - areaB, areaR, 0, areaB - areaR)
        | otherwise                = (2 * (x2 - x1) - areaR, areaB, areaR - areaB, 0)
      where
        areaB = antiB x2 - antiB x1
        areaR = antiR x2 - antiR x1

    antiB x = x - (cos (a*x + b) / a)
    antiR x = x - (cos (c*x + d) / c)

    family :: (Double -> Double) -> [Double]
    family f
        | f 0 < l   = takeWhile (<=r) (dropWhile (<l) (map f up))
        | f 0 > r   = reverse (takeWhile (>=l) (dropWhile (>r) (map f dw)))
        | otherwise = reverse (takeWhile (>=l) (map f dw)) <> takeWhile (<=r) (map f up)
      where
        (up,dw) | f 1 > f 0 = ([1..],[0,-1..])
                | otherwise = ([0,-1..],[1..])

format :: (Double,Double,Double,Double) -> String
format (gr,ye,re,bl) = printf "%.6f %.6f %.6f %.6f" gr ye re bl

(++++) :: (Double,Double,Double,Double) -> (Double,Double,Double,Double) -> (Double,Double,Double,Double)
(++++) (gr1,ye1,re1,bl1) (gr2,ye2,re2,bl2) = (gr1+gr2,ye1+ye2,re1+re2,bl1+bl2)

merge :: [Double] -> [Double] -> [Double]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | abs (x - y) < 0.000001 = merge (x:xs) ys
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
