{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C
import           Control.Arrow            ((>>>))
import           Data.Bool                (bool)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (readDouble >>> cantor 20 0 1 >>> bool "NON-MEMBER" "MEMBER")
        >>> C.unlines
        >>> C.putStr
    )

cantor :: Int -> Double -> Double -> Double -> Bool
cantor 0 _ _ _ = True
cantor i lo hi x | x >= hi - third = cantor (i-1) (hi-third) hi x
                 | x <= lo + third = cantor (i-1) lo (lo+third) x
                 | otherwise = False
    where
        third = (hi - lo) / 3

readDouble :: C.ByteString -> Double
readDouble s | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where Just (int, r1) = C.readInt s
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))