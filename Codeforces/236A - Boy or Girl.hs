import           Control.Arrow ((>>>))
import           Data.Bool     (bool)
import           Data.Char     (ord)
import qualified Data.IntSet   as S

main :: IO ()
main = getLine >>= (
            map ord
        >>> S.fromList
        >>> S.size
        >>> odd
        >>> bool "CHAT WITH HER!" "IGNORE HIM!"
        >>> putStrLn
    )
