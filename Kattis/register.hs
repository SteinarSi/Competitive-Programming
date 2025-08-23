import           Control.Arrow ((>>>))

main :: IO ()
main = getLine >>= (
            words
        >>> map read
        >>> zipWith (*) (scanl (*) 1 registers)
        >>> sum
        >>> (product registers -)
        >>> pred
        >>> print
    )

registers :: [Int]
registers = [2,3,5,7,11,13,17,19]
