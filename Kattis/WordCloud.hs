import           Control.Arrow         ((&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map C.words
        >>> parse
        >>> zipWith solve [1..]
        >>> concat
        >>> putStr
    )

solve :: Int -> (Int,[(Int,Int)]) -> String
solve i (m,xs) = printf "CLOUD %d: %d\n" i (spread 0 (-10) xs)
  where
    cmax = maximum (map snd xs)

    spread :: Int -> Int -> [(Int,Int)] -> Int
    spread h _ [] = h
    spread h w ((l,f):ys) | w + width + 10 <= m = spread (max h font) (w + width + 10) ys
                          | otherwise = h + spread 0 (-10) ((l,f):ys)
      where
        width = ceiling (9/16 * fromIntegral (l*font))
        font = 8 + ceiling (fromIntegral (40*(f-4)) / fromIntegral (cmax-4))

parse :: [[C.ByteString]] -> [(Int,[(Int,Int)])]
parse [] = []
parse [_] = []
parse ([w,n]:xs) = splitAt (readInt n) xs
    & (map ((head >>> C.length) &&& (last >>> readInt)) >>> (readInt w,)) *** parse
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
