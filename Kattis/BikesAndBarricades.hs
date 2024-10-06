import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (catMaybes, fromJust, listToMaybe)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words
            >>> map readInt
            >>> parse
            >>> intersect)
        >>> catMaybes
        >>> filter (>0)
        >>> (\xs -> bool (minimum xs) (-1) (null xs))
        >>> print
    )

parse :: [Int] -> ((Int,Int),(Int,Int))
parse [x1,y1,x2,y2] = (min a b, max a b)
    where
        a = (x1,y1)
        b = (x2,y2)

intersect :: ((Int,Int),(Int,Int)) -> Maybe Double
intersect ((x1,y1),(x2,y2)) | x1 <= 0 && x2 >= 0 = Just (base + ratio * delta)
                            | otherwise = Nothing
    where
        base  = fromIntegral y1
        ratio = fromIntegral (-x1) / fromIntegral (x2-x1)
        delta = fromIntegral (y2-y1)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
