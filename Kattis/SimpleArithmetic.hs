import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Data.Ratio    (denominator, numerator, (%))
import           Text.Printf   (printf)

scale :: Integer
scale = 10^6

main :: IO ()
main = do
    [a,b,c] <- getContents <&> (words >>> map read)
    let ans = (a*b) % c
        v = numerator ans * scale `div` denominator ans
        (int,dec) = quotRem v scale
    printf "%d.%.6d\n" int (abs dec)
