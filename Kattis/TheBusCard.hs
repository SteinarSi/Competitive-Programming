import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = getContents >>= (
            read
        >>> (+99)
        >>> (`div` 100)
        >>> (*100)
        >>> transactions [500,200,100]
        >>> print
    )

transactions :: [Int] -> Int -> Int
transactions _ 0 = 0
transactions [] _ = 1
transactions (d:ds) x = let (q,r) = quotRem x d
                        in  q + transactions ds r
