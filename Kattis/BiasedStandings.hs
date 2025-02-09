import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 2
        >>> parse
        >>> map (sort
            >>> zipWith ((-) >>> (>>> abs)) [1..]
            >>> sum
            >>> show)
        >>> unlines
        >>> putStr
    )

parse :: [C.ByteString] -> [[Int]]
parse [] = []
parse (x:xs) = splitAt (readInt x) xs
    & map (C.words >>> last >>> readInt)
        ***
      (drop 1 >>> parse)
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
