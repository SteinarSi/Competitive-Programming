import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (chr, isAlpha, ord)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.interact (
            C.lines
        >>> drop 1
        >>> solve
        >>> C.unlines
    )

solve :: [C.ByteString] -> [C.ByteString]
solve []       = []
solve (s:m:xs) = C.map (\x -> if isAlpha x then chr ((ord x - ord 'A' - readInt s) `mod` 26 + ord 'A') else x) m : solve xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
