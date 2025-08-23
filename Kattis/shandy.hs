import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (
            words
        >>> map read
        >>> minimum
        >>> (*2)
        >>> print
    )
