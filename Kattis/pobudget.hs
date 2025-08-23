import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> zip [1..]
        >>> filter (fst >>> even)
        >>> map (snd >>> readInt)
        >>> sum
        >>> format
        >>> putStrLn
    )

format :: Int -> String
format x = case compare x 0 of
        LT -> "Nekad"
        EQ -> "Lagom"
        GT -> "Usch, vinst"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
