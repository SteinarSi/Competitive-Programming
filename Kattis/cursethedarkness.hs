{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> split
        >>> mapM_ (uncurry solve >>> C.putStrLn)
    )

solve :: (Double,Double) -> [(Double,Double)] -> C.ByteString
solve xy xs | any (sqDist xy >>> (<=64)) xs = "light a candle"
            | otherwise                     = "curse the darkness"

split :: [C.ByteString] -> [((Double,Double), [(Double,Double)])]
split [] = []
split (xy:n:xs) = let (ys, zs) = splitAt (readInt n) xs
                  in  (readPoint xy, map readPoint ys) : split zs

sqDist :: (Double, Double) -> (Double, Double) -> Double
sqDist (x,y) (a,b) = abs (x-a)^^2 + abs (y-b)^^2

readPoint :: C.ByteString -> (Double,Double)
readPoint s = let x:y:_ = map readDouble (C.words s)
              in  (x, y)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

readDouble :: C.ByteString -> Double
readDouble s | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where Just (int, r1) = C.readInt s
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))
