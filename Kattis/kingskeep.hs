import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt >>> head &&& last)
        >>> solve
        >>> printf "%.6f\n"
    )

solve :: [(Int,Int)] -> Double
solve xs = map (\x -> sum (map (dist x) xs)) xs
    & minimum
    & (/ fromIntegral (length xs - 1))

dist :: (Int,Int) -> (Int,Int) -> Double
dist (a,b) (x,y) = sqrt (fromIntegral ((a-x)^2 + (b-y)^2))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
