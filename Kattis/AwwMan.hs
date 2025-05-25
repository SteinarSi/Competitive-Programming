{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map readInt
        >>> solve
        >>> C.unlines
        >>> C.putStr
    )

solve :: [Int] -> [C.ByteString]
solve [] = []
solve (n:m:a:b:d:xs) = ans : solve xs
  where
    ans | home <= (n-m) = "YES"
        | otherwise     = "NO"
    walk = (b-a) `mod` n
    home = (b + d + walk) `mod` n

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
