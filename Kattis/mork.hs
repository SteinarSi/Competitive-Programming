import           Control.Arrow ((>>>))
import           Data.Bool     (bool)

main :: IO ()
main = getContents >>= (
            words
        >>> map read
        >>> (`elem` [[0,0],[2,2]])
        >>> bool "Neibb" "Jebb"
        >>> putStrLn
    )
