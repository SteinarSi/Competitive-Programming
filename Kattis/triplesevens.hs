import           Control.Arrow ((>>>))
import           Data.Bool     (bool)

main :: IO ()
main = getContents >>= (
            lines
        >>> tail
        >>> map words
        >>> all (elem "7")
        >>> bool "0" "777"
        >>> putStrLn
    )
