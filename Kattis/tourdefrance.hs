import qualified Data.ByteString.Char8 as C
import Control.Arrow ((>>>))
import Data.Maybe (fromJust)
import Data.Ratio 
import Data.List (sort)
import Data.Function ((&))
import Text.Printf (printf)


main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> map (readInt >>> fromIntegral))
        >>> solve
        >>> C.concat
        >>> C.putStr
    )

solve :: [[Double]] -> [C.ByteString]
solve [] = []
solve [_] = []
solve (_:xs:ys:xss) = zipWith (/) (drop 1 ratios) ratios
        & maximum
        & printf "%.2f\n"
        & C.pack
        & (: solve xss)
    where
        ratios = map (/) ys <*> xs
                & sort

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst