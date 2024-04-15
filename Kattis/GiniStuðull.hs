import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readNum)

    let top = solve n 0 0 (sum xs) (sort xs)
        bot = 2 * n * sum xs

    print (nullSafeDiv top bot)

solve :: Double -> Double -> Double -> Double -> [Double] -> Double
solve _ _ _ _ [] = 0
solve n l lsum rsum (x:xs) = l * abs (avgl - x) + r * abs (avgr - x) + solve n (l+1) (lsum+x) (rsum-x) xs
    where r = n - l
          avgl = nullSafeDiv lsum l
          avgr = nullSafeDiv rsum r

readNum :: Num a => C.ByteString -> a
readNum = C.readInt >>> fromJust >>> fst >>> fromIntegral

nullSafeDiv :: (Eq a, Fractional a) => a -> a -> a
nullSafeDiv _ 0 = 0
nullSafeDiv a b = a / b
