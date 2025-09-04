import           Control.Arrow ((>>>))
import           Data.Bool     (bool)

main :: IO ()
main = interact (
            lines
        >>> drop 1
        >>> map (words >>> map read >>> \[a,b,c::Int] -> bool "Unordered" "Ordered" (a <= b && b <= c ||  a >= b && b >= c))
        >>> ("Gnomes:":)
        >>> unlines
    )
