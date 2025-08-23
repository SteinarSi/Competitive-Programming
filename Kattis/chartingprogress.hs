{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.List             (sortBy)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> splitOn ""
        >>> map (solve >>> C.unlines)
        >>> C.unlines
        >>> C.putStr
    )

solve :: [C.ByteString] -> [C.ByteString]
solve = C.transpose >>> sortBy (compare `on` (C.elemIndex '*' >>> fmap negate)) >>> C.transpose

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn p (x:xs) | p == x    = splitOn p xs
                 | otherwise = (x : takeWhile (/=p) xs) : splitOn p (dropWhile (/=p) xs)
