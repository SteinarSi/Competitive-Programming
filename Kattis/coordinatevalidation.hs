import           Control.Arrow ((>>>))
import           Data.Bool     (bool)

main :: IO ()
main = getContents >>= (
            lines
        >>> drop 1
        >>> map (words >>> map read >>> \[x,y] -> bool "INVALID" "VALID" (-180.0 <= x && x <= 180.0 && -90 <= y && y <= 90))
        >>> unlines
        >>> putStr
    )
