{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         (first, (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (partition)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> parse
        >>> map (uncurry solve >>> show)
        >>> unlines
        >>> putStr
    )

solve :: Int -> ([Int],[Int]) -> Int
solve l = uncurry (sol l)
  where
    sol :: Int -> [Int] -> [Int] -> Int
    sol r [] []     | r == l    = 0
                    | otherwise = 1
    sol r rs []     = 1 + sol l [] rs
    sol r rs (x:xs) | x <= r    = sol (r-x) rs xs
                    | otherwise = 1 + sol l (x:xs) rs

parse :: [C.ByteString] -> [(Int,([Int],[Int]))]
parse [] = []
parse (lm:xs) = let [l,m] = map readInt (C.words lm)
                    ((ls,rs),zs) = splitAt m xs
                        & first (map (C.readInt >>> fromJust)
                            >>> partition (snd >>> (==" right"))
                            >>> map fst *** map fst)
                in  (100 * l, (ls,rs)) : parse zs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
