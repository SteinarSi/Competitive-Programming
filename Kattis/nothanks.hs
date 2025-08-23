import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> map readInt
        >>> sort
        >>> nothanks 0
        >>> print
    )

nothanks :: Int -> [Int] -> Int
nothanks _ [] = 0
nothanks s (x:xs) | x == s    =     nothanks (x+1) xs
                  | otherwise = x + nothanks (x+1) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
