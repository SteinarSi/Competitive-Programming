import           Control.Arrow         ((>>>))
import           Data.Array            (Array, listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt >>> solve)
        >>> C.unlines
        >>> C.putStr
    )

solve :: [Int] -> C.ByteString
solve [i,n,v] = C.pack (printf "%d %d" i (pdc ! (n,v)))

pdc :: Array (Int,Int) Int
pdc = listArray rng (map f (range rng))
    where
        f :: (Int,Int) -> Int
        f (n,v) | v <= 0    = 1
                | v >= n    = 0
                | otherwise = (pdc ! (n-1,v-1) * (n-v) + pdc ! (n-1,v) * (v+1)) `mod` 1001113

        rng :: ((Int, Int), (Int, Int))
        rng = ((0,0),(100,99))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
