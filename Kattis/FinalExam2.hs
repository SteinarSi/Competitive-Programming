import Control.Arrow ((>>>))
import Control.Monad (ap)

main :: IO ()
main = getContents >>= (
            lines
        >>> ap (zipWith (==)) tail
        >>> filter id
        >>> length
        >>> print
    )
