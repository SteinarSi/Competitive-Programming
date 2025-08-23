import           Control.Arrow ((>>>))

main :: IO ()
main = getLine >>= (
            filter ('1'==)
        >>> length
        >>> print
    )
