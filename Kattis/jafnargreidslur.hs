import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [x,n,q] <- getLine <&> (words >>> map read)
    let rem = fromIntegral x
        p = fromIntegral q / 10000

        bin :: Int -> Double -> Double -> Double
        bin m lo hi
            | m == 0 = mi
            | otherwise = case mortgage p mi n rem of
                LT -> bin (m-1) mi hi
                EQ -> mi
                GT -> bin (m-1) lo mi
          where
            mi = (lo+hi) / 2

    print (bin 25 (rem / fromIntegral n) (rem / fromIntegral n + rem*p))

mortgage :: Double -> Double -> Int -> Double -> Ordering
mortgage p rate 0 rem = compare 0 rem
mortgage p rate n rem
    | rem < 0 = GT
    | rem' >= rem = LT
    | rem > fromIntegral n * rate = LT
    | otherwise = mortgage p rate (n-1) rem'
  where
    rem' = rem + rem * p - rate
