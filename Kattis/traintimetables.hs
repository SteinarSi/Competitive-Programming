import           Control.Arrow         (first, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> split
        >>> zipWith solve [1..]
        >>> unlines
        >>> putStr
    )

solve :: Int -> (Int, [(Int,Int)], [(Int,Int)]) -> String
solve i (t,a,b) = printf "Case #%d: %d %d" i (station (b,a)) (station (a,b))
    where
        station (x,y) = train (sort (map snd x)) (sort (map fst y))

        train :: [Int] -> [Int] -> Int
        train [] ys = length ys
        train _  [] = 0
        train (x:xs) (y:ys) | x + t <= y =     train xs ys
                            | otherwise  = 1 + train (x:xs) ys

split :: [C.ByteString] -> [(Int, [(Int,Int)], [(Int,Int)])]
split [] = []
split (t:nm:xs) = (readInt t, a, b) : split c
    where
        [n,m] = C.words nm
            & map readInt
        ((a,b),c) = splitAt (n+m) xs
            & first (map parseInterval >>> splitAt n)

parseInterval :: C.ByteString -> (Int, Int)
parseInterval xs = (from,to)
    where
        [from,to] = C.words xs
            & map parseTime

parseTime :: C.ByteString -> Int
parseTime x = let Just (hh,mm) = C.readInt x
              in  60 * hh + readInt (C.tail mm)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
