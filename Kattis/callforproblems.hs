import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (
            lines
        >>> tail
        >>> map read
        >>> filter odd
        >>> length
        >>> print
    )
