import           Control.Arrow ((>>>))
import           Control.Monad (ap)

main :: IO ()
main = getContents >>= (
            words
        >>> drop 1
        >>> map (read::String->Int)
        >>> ap (zipWith (<)) (drop 1)
        >>> filter id
        >>> length
        >>> print
    )
