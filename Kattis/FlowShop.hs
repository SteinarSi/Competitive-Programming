{-# LANGUAGE Strict #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (foldl', transpose)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map C.words
        >>> transpose
        >>> map (map readInt)
        >>> foldl' (process 0) (repeat 0)
        >>> map (show >>> C.pack)
        >>> C.unwords
        >>> C.putStrLn
    )

process :: Int -> [Int] -> [Int] -> [Int]
process _ [] _ = []
process _ _ [] = []
process p (x:xs) (y:ys) = next : process next xs ys
    where next = max p x + y

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
