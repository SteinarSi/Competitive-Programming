import           Control.Arrow ((>>>))
import           Control.Monad (ap)

main :: IO ()
main = getContents >>= (
            words
        >>> tail
        >>> map (read::String->Int)
        >>> ap (zipWith (<)) tail
        >>> filter id
        >>> length
        >>> succ
        >>> print
    )
