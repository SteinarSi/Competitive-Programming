import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> C.transpose
        >>> judge
        >>> print
    )

judge :: [C.ByteString] -> Int
judge [] = 0
judge (x:xs) | empty x   = judge xs
             | otherwise = 1 + judge (dropWhile (empty >>> not) xs)
    where empty = C.all ('_'==)
