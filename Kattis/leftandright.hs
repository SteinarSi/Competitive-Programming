import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = getLine >> getLine >>= (
            (<> "R")
        >>> solve 1 0
        >>> map show
        >>> unlines
        >>> putStr
    )

solve :: Int -> Int -> String -> [Int]
solve a s []       = []
solve a s ('L':xs) = solve a (s+1) xs
solve a s ('R':xs) = [a+s,a+s-1..a] <> solve (a+s+1) 0 xs
