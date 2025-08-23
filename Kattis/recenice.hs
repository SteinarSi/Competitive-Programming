{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C
import           Control.Arrow            ((>>>), second)
import           Data.Function            ((&))
import           Data.List                (find)
import           Data.Maybe               (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> rečenice
        >>> C.putStrLn
    )

rečenice :: [C.ByteString] -> C.ByteString
rečenice xs = let (before, _:after) = span (/= "$") xs
                  len = map C.length xs
                      & sum
                      & pred
                  number = numbers
                      & find (second (C.length >>> (+len)) >>> uncurry (==))
                      & fromJust
                      & snd
              in  C.unwords (before ++ [number] ++ after)

numbers :: [(Int, C.ByteString)]
numbers = ones ++ teens ++ (map (<>) tens <*> ones)
        & (map (<>) hundreds <*>)
        & tail
        & zip [1..]
    where
        ones :: [C.ByteString]
        ones = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

        teens :: [C.ByteString]
        teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

        tens :: [C.ByteString]
        tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

        hundreds :: [C.ByteString]
        hundreds = "" : map (<>"hundred") (tail ones)
