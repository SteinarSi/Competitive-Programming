import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (C.lines
        >>> mapM_ (C.words
            >>> map readInt
            >>> solve
            >>> print        
        )
    )

solve :: [Int] -> Int
solve [m, p, l, e, r, s, 0] = m
solve [m, p, l, e, r, s, n] = solve [p `div` s, l `div` r, e * m, e, r, s, n-1]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst