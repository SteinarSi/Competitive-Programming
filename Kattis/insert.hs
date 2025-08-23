{-# LANGUAGE Strict, OverloadedStrings #-}

import Control.Monad (when, replicateM_)
import Data.List (genericLength)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getLine >>= flip when (C.getLine >>= print . combs . map (fromIntegral . fst . fromJust . C.readInt) . C.words >> main) . (/="0")

combs :: [Integer] -> Integer
combs [] = 1
combs (x:xs) = combs smol * combs bigg * choose (genericLength xs) (genericLength smol)
    where smol = filter (< x) xs
          bigg = filter (>=x) xs

choose :: Integer -> Integer -> Integer
choose n 0 = 1
choose n k | 2 * k > n = choose n (n-k)
           | otherwise = div (n * choose (n-1) (k-1)) k
