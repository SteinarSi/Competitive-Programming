import           Control.Arrow ((>>>))
import           Data.Bool     (bool)
import           Data.Char     (digitToInt)

main :: IO ()
main = getLine >>= (
            map digitToInt
        >>> sum
        >>> (`mod` 3)
        >>> (==0)
        >>> bool "Neibb" "Jebb"
        >>> putStrLn
    )
