import           Control.Arrow ((>>>))
import           Control.Monad (ap)
import           Data.Bool     (bool)

main :: IO ()
main = interact (
            init
        >>> map (`elem` "yuiophjklnm")
        >>> ap (zipWith (/=)) (drop 1)
        >>> and
        >>> bool "no" "yes"
    )
