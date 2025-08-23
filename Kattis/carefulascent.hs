import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    xy:_:xs <- C.getContents <&> C.lines

    let [x,y] = map readDouble (C.words xy)
        shields = sort (map (C.words >>> map readDouble >>> \[l,u,f] -> (l,u,f)) xs)

    printf "%.6f\n" (solve (x,y) shields)

solve :: (Double,Double) -> [(Double,Double,Double)] -> Double
solve (x,y) shields = search 100 (-10000,10000)
    where
        search :: Int -> (Double,Double) -> Double
        search i (lo,hi) | i <= 0    = mi
                         | otherwise = case on compare (\v -> ascend v (0,0) shields) mi (mi+0.0000001) of
                                LT -> search (i-1) (lo,mi)
                                EQ -> mi
                                GT -> search (i-1) (mi,hi)
            where mi = (lo + hi) / 2

        ascend :: Double -> (Double,Double) -> [(Double,Double,Double)] -> Double
        ascend v (width,height) [] = abs (x-(width + (y-height) * v))
        ascend v (width,height) ((l,u,f):xs) = ascend v (width + v * (l-height) + (v*f) * (u-l),u) xs

readDouble :: C.ByteString -> Double
readDouble s | C.head s == '-' = negate (readDouble (C.tail s))
             | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where (int, r1) | C.head s == '.' = (0, s)
                    | otherwise = fromJust (C.readInt s)
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
