import           Control.Arrow      ((>>>))
import           Data.Bool          (bool)
import qualified Data.IntMap.Strict as M

main :: IO ()
main = getContents >>= (
            words
        >>> drop 1
        >>> map (read >>> (,1))
        >>> M.fromListWith (+)
        >>> M.assocs
        >>> map (\(t,c) -> bool c (c-t) (c>=t))
        >>> sum
        >>> print
    )
