import           Control.Arrow ((>>>))
import           Data.Bool     (bool)

main :: IO ()
main = interact (
            lines
        >>> init
        >>> map (words >>> map read >>> \[a,b::Int] -> bool "Less" "More" (a > b))
        >>> unlines
    )
