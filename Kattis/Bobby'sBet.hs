{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words
            >>> map readInt
            >>> (\(r:s:x:y:w:_) -> solve r s x y w))
        >>> C.unlines
        >>> C.putStr
    )

solve :: Int -> Int -> Int -> Int -> Int -> C.ByteString
solve r s x y w = map exact [x..y]
        & sum
        & (*fromIntegral w)
        & (>1)
        & bool "no" "yes"
    where
        p = fromIntegral (s - r + 1) / fromIntegral s
        q = 1 - p

        exact i = fromIntegral (y `choose` i) * p^i * q^(y-i)

choose :: Int -> Int -> Int
choose n k | k > n           = 0
           | k == 0          = 1
           | k > (n `div` 2) = n `choose` (n-k)
           | otherwise       = n * ((n-1) `choose` (k-1)) `div` k

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
