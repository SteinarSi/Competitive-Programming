import           Control.Arrow ((&&&), (>>>))
import           Data.Functor  ((<&>))
import           Text.Printf   (printf)

main :: IO ()
main = do
    xs <- getContents <&> (lines >>> map (words >>> map read >>> head &&& last))
    let (x,y) = median xs 1000 (pi,pi)
    printf "%f %f\n" x y

median :: [(Double,Double)] -> Int -> (Double,Double) -> (Double,Double)
median xs i (x,y) | i == 0 || any (< 0.0001) dist = (x,y)
                  | otherwise                     = median xs (i-1) (
                        sum (zipWith (/) (map fst xs) dist) / den,
                        sum (zipWith (/) (map snd xs) dist) / den
                    )
  where
    dist = map (\(x',y') -> sqrt ((x-x')^2 + (y-y')^2)) xs
    den  = sum (map (1/) dist)
