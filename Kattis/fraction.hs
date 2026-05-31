import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.Ratio    (Rational, denominator, numerator, (%))

main :: IO ()
main = do
    [_,x,y] <- getContents <&> (lines >>> map (words >>> map read >>> parse))
    [x+y,x-y,x*y,x/y]
        & map (format >>> map show >>> unwords)
        & unlines
        & putStr

parse :: [Integer] -> Rational
parse [x]    = x % 1
parse (x:xs) = x % 1 + invert (parse xs)

invert :: Rational -> Rational
invert x = denominator x % numerator x

format :: Rational -> [Integer]
format x
    | n == 0 = []
    | d == 1 = [n]
    | otherwise = q : format (d % r)
  where
    (q,r) = quotRem n d
    n = numerator x
    d = denominator x
