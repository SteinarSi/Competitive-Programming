{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         (second, (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> split
        >>> map (uncurry solve)
        >>> C.unlines
        >>> C.putStr
    )

solve :: Int -> [(Int,C.ByteString)] -> C.ByteString
solve _ []                           = "Stan may be honest"
solve a ((g,"too high"):xs) | g <= a = "Stan is dishonest"
solve a ((g,"too low" ):xs) | g >= a = "Stan is dishonest"
solve a (_             :xs)          = solve a xs

split :: [C.ByteString] -> [(Int, [(Int,C.ByteString)])]
split [] = []
split xs = span (/="right on") xs
        & pairs *** (drop 1 >>> split)
        & uncurry (:)
    where
        pairs :: [C.ByteString] -> (Int, [(Int,C.ByteString)])
        pairs [a]      = (readInt a, [])
        pairs (g:r:ys) = second ((readInt g, r) :) (pairs ys)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
