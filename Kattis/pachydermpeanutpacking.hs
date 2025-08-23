{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (find)
import           Data.Maybe            (fromJust)

type Box = ((Double,Double,Double,Double), C.ByteString)
type Nut = ((Double,Double), C.ByteString)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> reverse)
        >>> parseCases
        >>> map (uncurry solve)
        >>> C.intercalate "\n\n"
        >>> C.putStrLn
    )

solve :: [Box] -> [Nut] -> C.ByteString
solve boxes = map search >>> C.intercalate "\n"
    where
        search :: Nut -> C.ByteString
        search (pos,size) = size <> case find (fst >>> within pos) boxes of
            Just (_, size') | size == size' -> " correct"
                            | otherwise     -> " " <> size'
            Nothing                         -> " floor"

parseCases :: [[C.ByteString]] -> [([Box], [Nut])]
parseCases [] = []
parseCases [["0"]] = []
parseCases [_] = []
parseCases ([n]:xs) = let (boxes, [m]:rest) = splitAt (readInt n) xs
                          (nuts, rest')     = splitAt (readInt m) rest
                      in  (
                            map parseBox boxes,
                            map parseNut nuts
                          ) : parseCases rest'
    where
        parseBox :: [C.ByteString] -> Box
        parseBox (size:pos) = let [y2,x2,y1,x1] = map readDouble pos
                              in  ((x1,y1,x2,y2),size)

        parseNut :: [C.ByteString] -> Nut
        parseNut (size:pos) = let [y,x] = map readDouble pos
                              in  ((x,y), size)


within :: (Double,Double) -> (Double,Double,Double,Double) -> Bool
within (p,q) (x1,y1,x2,y2) = and [
        x1 <= p,
        p  <= x2,
        y1 <= q,
        q  <= y2
    ]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

readDouble :: C.ByteString -> Double
readDouble s | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where Just (int, r1) = C.readInt s
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))
