{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (guard)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> map readInt)
        >>> split
        >>> map (possible >>> bool "NO" "YES")
        >>> C.unlines
        >>> C.putStr
    )

possible :: ([Int],[Int]) -> Bool
possible (xs,ys) = not $ null $ do
        (i,x) <- zip [1..] xs
        (j,y) <- zip [1..] ys
        guard (x == y && i /= j)
        pure ()

split :: [[Int]] -> [([Int],[Int])]
split []          = []
split (xs:ys:xss) = (xs,ys) : split xss

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
