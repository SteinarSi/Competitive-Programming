import           Control.Arrow ((>>>))
import           Data.Char     (isAlpha)

main :: IO ()
main = getLine >>= (
            filter isAlpha
        >>> length
        >>> print
    )
