import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> solve
        >>> C.unlines
        >>> C.putStr
    )

solve :: [C.ByteString] -> [C.ByteString]
solve [] = []
solve (n:b:xs) = (b `C.snoc` ':') : sort (filter (\y -> C.length y -C.count ' ' y == C.length b - C.count ' ' b) ys) <> solve zs
  where
    (ys,zs) = splitAt (readInt n) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
