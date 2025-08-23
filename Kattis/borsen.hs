import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    _:f:xs <- C.getContents <&> (C.words >>> map readDouble)
    print $ speculate f 0 100 xs

speculate :: Double -> Double -> Double -> [Double] -> Double
speculate _ shares liquid [] = liquid
speculate fee shares liquid (x:xs) = speculate fee shares' liquid' xs
  where
    shares' = max shares ((liquid-fee)/x)
    liquid' = max liquid (shares*x-fee)

readDouble :: C.ByteString -> Double
readDouble s | C.head s == '-' = negate (readDouble (C.tail s))
             | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where (int, r1) | C.head s == '.' = (0, s)
                    | otherwise = fromJust (C.readInt s)
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))
