import           Control.Arrow         ((***), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (sort)
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> parse
        >>> map solve
        >>> unlines
        >>> putStr
    )

parse :: [C.ByteString] -> [[C.ByteString]]
parse [] = []
parse (x:xs) = splitAt (readInt x) xs
        & sort *** parse
        & uncurry (:)

solve :: [C.ByteString] -> String
solve xs = zipWith C.isPrefixOf xs (tail xs)
        & or
        & bool "YES" "NO"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
