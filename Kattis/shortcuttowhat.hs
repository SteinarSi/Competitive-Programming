import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (
            read
        >>> (+5)
        >>> (*3)
        >>> subtract 10
        >>> print
    )
