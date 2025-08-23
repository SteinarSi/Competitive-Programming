import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Data.Ratio            (Rational, denominator, numerator, (%))

main :: IO ()
main = do
    _:xs <- C.getContents <&> (C.words >>> map readInteger)

    let best = max (stopCounting 0 0 0 xs) (stopCounting 0 0 0 (reverse xs))

    print (fromIntegral (numerator best) / fromIntegral (denominator best) :: Double)

stopCounting :: Rational -> Integer -> Integer -> [Integer] -> Rational
stopCounting best n total []     = best
stopCounting best n total (x:xs) = stopCounting (max best ((total+x) % (n+1))) (n+1) (total+x) xs

readInteger :: C.ByteString -> Integer
readInteger = C.readInteger >>> fromJust >>> fst
