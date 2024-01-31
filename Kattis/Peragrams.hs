import           Control.Arrow ((>>>))
import           Data.List     (foldl')
import           Data.Map      (Map, elems, empty, insertWith)

main :: IO ()
main = getLine >>= (
            foldl' (\m c -> insertWith (+) c 1 m) empty
        >>> elems
        >>> filter odd
        >>> length
        >>> pred
        >>> max 0
        >>> print
    )
