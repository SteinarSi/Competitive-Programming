{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>), (&&&))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt, isDigit, ord)
import           Data.Ord              (comparing)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> split
        >>> map solve
        >>> C.unlines
        >>> C.putStr
    )

solve :: (C.ByteString,C.ByteString) -> C.ByteString
solve (a,b) | compare a b == comparing parse a b = "YES"
            | otherwise                          = "NO"

parse :: C.ByteString -> (Int,[Int])
parse = C.length &&& C.foldr (f >>> (:)) []
    where
        f x | isDigit x            = digitToInt x
            | 'a' <= x && x <= 'z' = ord x - ord 'a' + 10
            | otherwise            = ord x - ord 'A' + 36

split :: [a] -> [(a,a)]
split [] = []
split (x:y:xs) = (x,y) : split xs
