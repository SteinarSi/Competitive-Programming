import Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (
            read
        >>> enumFromTo 1
        >>> map show
        >>> unlines
        >>> putStr
    )
