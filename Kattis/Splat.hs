{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow            (first, (>>>), (&&&))
import qualified Data.ByteString.Char8 as C
import           Data.Function            ((&))
import           Data.List                (find)
import           Data.Maybe               (fromJust)

type Point = (Double,Double)
type Splat = ((Point,Double), C.ByteString)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> parse
        >>> map (uncurry solve)
        >>> C.concat
        >>> C.putStr
    )

parse :: [C.ByteString] -> [([Splat], [Point])]
parse [] = []
parse (n:xs) = let (splats, m:ys) = splitAt (readInt n) xs
                        & first (map (C.words >>> ((init >>> map readDouble >>> (\(x:y:v:_) -> ((x,y), v / pi))) &&& last)))
                   (queries, zs) = splitAt (readInt m) ys
                        & first (map (C.words >>> map readDouble >>> (head &&& last)))
               in  (reverse splats, queries) : parse zs

solve :: [Splat] -> [Point] -> C.ByteString
solve splats = map color >>> C.unlines
    where
        color :: Point -> C.ByteString
        color p = find (\((q,r),_) -> sqDist p q <= r) splats
                & maybe "white" snd

sqDist :: Point -> Point -> Double
sqDist (x,y) (a,b) = abs (x-a)**2 + abs (y-b)**2

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

readDouble :: C.ByteString -> Double
readDouble s |  C.head s == '-' = negate (readDouble (C.tail s))
             | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where (int, r1) | C.head s == '.' = (0, s)
                    | otherwise = fromJust (C.readInt s)
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))
