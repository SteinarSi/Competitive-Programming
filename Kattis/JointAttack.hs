import           Control.Arrow ((>>>))
import           Data.Ratio    (Rational, denominator, numerator)

main :: IO ()
main = getContents >>= (
            words
        >>> tail
        >>> map read
        >>> continue
        >>> format
        >>> putStrLn
    )

continue :: [Integer] -> Rational
continue []     = 0
continue [x]    = fromInteger x
continue (x:xs) = fromInteger x + 1 / continue xs

format :: Rational -> String
format x = show (numerator x) ++ "/" ++ show (denominator x)
