import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (
            lines
        >>> drop 1
        >>> map (words >>> map read >>> \[h,b,k] -> k * max 0 (b-h))
        >>> sum
        >>> print
    )
