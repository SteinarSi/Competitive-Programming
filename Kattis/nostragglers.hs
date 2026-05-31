{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map C.words
        >>> solve 0
        >>> putStrLn
    )

solve :: Int -> [[C.ByteString]] -> String
solve s [] | s > 0     = show s
           | otherwise = "NO STRAGGLERS"
solve s ([_,d,i]:xs)   = solve (s + bool id negate (d == "OUT") (readInt i)) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
