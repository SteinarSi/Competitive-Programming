import           Control.Arrow         (first, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [s,_,w]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readDouble))
    print (solve s (sort (map (light w) xs)) 0)

solve :: Double -> [(Double,Double)] -> Double -> Int
solve s xs c
    | c >= s    = 0
    | otherwise = 1 + solve s zs r
  where
    (r,zs) = span (fst >>> (<=c)) xs
        & first (map snd >>> maximum)

light :: Double -> [Double] -> (Double,Double)
light w [s,h,l] = (s-x,s+x)
  where
    x = sqrt (max 0 (l / (4*pi*w) - h^2))

readDouble :: C.ByteString -> Double
readDouble s | C.head s == '-' = negate (readDouble (C.tail s))
             | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where (int, r1) | C.head s == '.' = (0, s)
                    | otherwise = fromJust (C.readInt s)
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))
