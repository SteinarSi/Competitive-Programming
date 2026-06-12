import           Control.Arrow ((>>>))
import           Data.Bool     (bool)
import           Data.Function ((&))

main :: IO ()
main = concatMap year [1901..2000]
    & scanl ((+) >>> (>>>(`mod` 7))) ((0 + sum (year 1900)) `mod` 7)
    & filter (==6)
    & length
    & print

year :: Int -> [Int]
year y = [31,28 + bool 0 1 (y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)) ,31,30,31,30,31,31,30,31,30,31]
