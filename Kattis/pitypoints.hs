import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (
            words
        >>> drop 1
        >>> map read
        >>> sum
        >>> print
    )
