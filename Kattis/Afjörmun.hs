import           Control.Arrow ((>>>))
import           Data.Char     (toLower, toUpper)

main :: IO ()
main = getContents >>= (
            lines
        >>> tail
        >>> map (\(x:xs) -> toUpper x : map toLower xs)
        >>> mapM_ putStrLn
    )
