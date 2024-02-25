import           Control.Arrow ((>>>))
import           Data.Bool     (bool)

main :: IO ()
main = getLine >>= (
            words
        >>> map read
        >>> (\[r,f] -> f / r)
        >>> round
        >>> even
        >>> bool "down" "up"
        >>> putStrLn
    )
