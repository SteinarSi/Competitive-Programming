{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((&&&), (***), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (intercalate)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> parse
        >>> map (map (uncurry solve) >>> validate >>> C.unlines)
        >>> C.intercalate "\n"
        >>> C.putStr
    )

validate :: [C.ByteString] -> [C.ByteString]
validate [] = []
validate (x:xs)
    | all (C.length >>> (== C.length x)) xs = x:xs
    | otherwise = x:xs <> ["Error decoding image"]

solve :: Bool -> [Int] -> C.ByteString
solve _ []     = ""
solve c (x:xs) = C.replicate x (bool '.' '#' c) <> solve (not c) xs

parse :: [C.ByteString] -> [[(Bool,[Int])]]
parse []     = []
parse ["0"]  = []
parse (x:xs) = splitAt (readInt x) xs
    & map (C.words >>> (head >>> (=="#")) &&& (drop 1 >>> map readInt)) *** parse
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
