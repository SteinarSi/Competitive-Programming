import           Control.Arrow ((>>>))
import           Data.Array    (Array, listArray, (!))
import           Text.Printf   (printf)

main :: IO ()
main = interact $
    read >>> chance >>> (1-) >>> printf "%.6f\n"

chance :: Integer -> Double
chance n | n > 50    = 1 / e
         | otherwise = fromIntegral (subfac n) / fromIntegral (fac ! n)

subfac :: Integer -> Integer
subfac n = round (fromIntegral (fac ! n) / e)

fac :: Array Integer Integer
fac = listArray (0,50) (1 : map (\x -> x * fac ! (x-1)) [1..50])

e :: Double
e = 2.718281828459045
