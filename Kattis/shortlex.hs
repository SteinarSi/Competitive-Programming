import           Control.Arrow         ((&&&), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (findIndex)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (readInt >>> solulu)
        >>> C.unlines
        >>> C.putStr
    )

solulu :: Int -> C.ByteString
solulu x = digits (x - 2^len + 1)
        & reverse
        & C.pack
        & pad
    where
        len :: Int
        len = findIndex (>x) lengths
            & fromJust
            & pred

        pad :: C.ByteString -> C.ByteString
        pad xs = C.replicate (len - C.length xs) '0' <> xs

        digits :: Int -> [Char]
        digits 0 = "0"
        digits 1 = "1"
        digits p = p
            & (even >>> bool '1' '0')
                &&&
              ((`div` 2) >>> digits)
            & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

lengths :: [Int]
lengths = map (2^) [0..]
    & scanl (+) 0
