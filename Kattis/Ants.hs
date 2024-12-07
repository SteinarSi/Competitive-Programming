import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> map readInt
        >>> split
        >>> map solve
        >>> C.unlines
        >>> C.putStr
    )

solve :: (Int,[Int]) -> C.ByteString
solve (l,xs) = C.pack (show (maximum (map (\x -> min x (l-x)) xs)) <> " " <> show (max (maximum xs) (l - minimum xs)))

split :: [Int] -> [(Int,[Int])]
split [] = []
split (l:n:xs) = splitAt n xs
        & (l,) *** split
        & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
