import           Control.Arrow ((>>>))
import           Data.Bool     (bool)

main :: IO ()
main = getLine >>= (
            words
        >>> map read
        >>> (\[a,b,c] -> lcm a b <= c)
        >>> bool "no" "yes"
        >>> putStrLn
    )
