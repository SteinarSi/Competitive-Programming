import           Control.Arrow ((>>>))
import           Data.Function (on)
import           Data.List     (maximumBy)

main :: IO ()
main = getContents >>= (
            lines
        >>> drop 1
        >>> filter (words >>> \[x,y] -> x==y)
        >>> maximumBy (compare `on` (words >>> last >>> (read::String->Int)))
        >>> putStrLn
    )
