{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> solve
        >>> C.putStrLn
    )

solve :: [C.ByteString] -> C.ByteString
solve [] = "Anywhere is fine I guess"
solve (k:name:xs) | elem "pea soup" menu && elem "pancakes" menu = name
                  | otherwise = solve ys
    where (menu, ys) = splitAt (readInt k) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
