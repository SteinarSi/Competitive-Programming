import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.List             (transpose)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map C.head)
        >>> transpose
        >>> map (spread False)
        >>> sum
        >>> print
    )

spread :: Bool -> [Char] -> Int
spread _ [] = 0
spread i xs = case zs of
    []      -> bool l 0 i
    '#':zs' -> 1 + bool l 0 i + spread False zs'
    'X':zs' -> spread True zs'
  where
    (ys,zs) = span (=='O') xs
    l = length ys
