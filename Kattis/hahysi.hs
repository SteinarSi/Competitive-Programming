import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (
        words
    >>> map (read
        >>> pred
        >>> gauss
        >>> modB)
    >>> product
    >>> modB
    >>> print)

modB :: Integer -> Integer
modB = (`mod` (10^9 + 7))

gauss :: Integer -> Integer
gauss n = (n * (n+1)) `div` 2
