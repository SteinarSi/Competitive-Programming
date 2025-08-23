{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (ap)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [s, t, n] <- readInts
    w:walks <- readInts
    rides <- readInts
    intervals <- readInts
    C.putStrLn $ if solve (s+w) (zip3 walks rides intervals) <= t
        then "yes"
        else "no"

solve :: Int -> [(Int,Int,Int)] -> Int
solve time [] = time
solve time ((walk,ride,interval):xs) = solve (time + (interval - time `mod` interval) `mod` interval + ride + walk) xs

readInts :: IO [Int]
readInts = fmap (C.words >>> map (C.readInt >>> fromJust >>> fst)) C.getLine
