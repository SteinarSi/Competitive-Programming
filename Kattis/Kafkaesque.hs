import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.List             (sortBy)
import           Data.Maybe            (fromJust)

import           Debug.Trace           (traceShow)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> map readInt
        >>> zip [1..]
        >>> sortBy (compare `on` snd)
        >>> map fst
        >>> kafka 1 []
        >>> print
    )

kafka :: Int -> [Int] -> [Int] -> Int
kafka _ [] [] = 1
kafka next seen [] = 1 + kafka next [] (reverse seen)
kafka next seen (x:xs) | x == next = kafka (next+1) seen xs
                       | otherwise = kafka next (x:seen) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
