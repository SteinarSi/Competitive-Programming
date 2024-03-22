import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = getContents >>= (
            lines
        >>> tail
        >>> mapM_ (solve [] >>> putStrLn)
    )

solve :: String -> String -> String
solve "" ""              = "YES"
solve _ ""               = "NO"
solve bag ('.':xs)       = solve bag xs
solve ('$':bag) ('b':xs) = solve bag xs
solve _ ('b':xs)         = "NO"
solve ('|':bag) ('t':xs) = solve bag xs
solve _ ('t':xs)         = "NO"
solve ('*':bag) ('j':xs) = solve bag xs
solve _ ('j':xs)         = "NO"
solve bag (x:xs)         = solve (x:bag) xs
