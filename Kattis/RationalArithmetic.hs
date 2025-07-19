{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           Data.Ratio            (denominator, numerator, (%))
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> solve)
        >>> unlines
        >>> putStr
    )

solve :: [C.ByteString] -> String
solve [a,b,o,c,d] = printf "%d / %d" (numerator z) (denominator z)
  where
    x = readInt a % readInt b
    y = readInt c % readInt d
    z = (case o of
        "+" -> (+)
        "-" -> (-)
        "*" -> (*)
        "/" -> (/)) x y

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
