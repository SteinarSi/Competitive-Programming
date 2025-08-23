import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt >>> head &&& last)
        >>> solve
        >>> show
        >>> putStrLn
    )

solve :: [(Int,Int)] -> Int
solve lights = fromJust $ find (\t -> all (green t) lights) [0..]
  where
    green :: Int -> (Int,Int) -> Bool
    green t (r,g) = t `mod` (r+g) >= r

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
