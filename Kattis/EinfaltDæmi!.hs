import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (
            read
        >>> enumFromTo 1
        >>> product
        >>> print
    )
