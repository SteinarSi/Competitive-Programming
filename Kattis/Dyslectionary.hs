{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.List             (intercalate, intersperse, sortBy)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> splitOn ""
        >>> map (solve >>> format)
        >>> C.intercalate "\n\n"
        >>> C.putStrLn
    )

format :: [C.ByteString] -> C.ByteString
format xs = C.init (C.unlines p)
    where l = maximum (map C.length xs)
          p = map (\x -> C.replicate (l - C.length x) ' ' <> x) xs

solve :: [C.ByteString] -> [C.ByteString]
solve = sortBy (compare `on` C.reverse)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn p (x:xs) | p == x    = splitOn p xs
                 | otherwise = (x : takeWhile (/=p) xs) : splitOn p (dropWhile (/=p) xs)
