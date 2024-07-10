{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow            ((>>>), (***))
import           Data.Bool                (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function            ((&))
import           Data.Maybe               (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> map (C.readInt >>> fromJust >>> fst)
        >>> split
        >>> map (uncurry solve >>> bool "NO" "YES")
        >>> C.unlines
        >>> C.putStr
    )

split :: [Int] -> [(Integer,[Integer])]
split [] = []
split (n:xs) = splitAt n xs 
    & (map fromIntegral >>> (,) (fromIntegral n)) *** split
    & uncurry (:)

solve :: Integer -> [Integer] -> Bool
solve n xs = sum xs `mod` n == 0
