{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((***), (>>>))
import           Data.Bits             (shiftR)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isDigit)
import           Data.Maybe            (fromJust)

type Matrix = (Int,Int,Int,Int)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.spanEnd isDigit
            >>> (<>" ")
                    ***
                (readInt >>> solve >>> show >>> C.pack)
            >>> uncurry (<>))
        >>> C.unlines
        >>> C.putStr
    )

solve :: Int -> Int
solve n | n <= 1    = 1
        | otherwise = let (_,_,x,_) = multiply (power (0,1,1,1) (n-2)) (1,0,1,0)
                      in  x

power :: Matrix -> Int -> Matrix
power m 0 = (1,0,0,1)
power m e | odd e     = multiply m (power (multiply m m) (shiftR e 1))
          | otherwise =             power (multiply m m) (shiftR e 1)

multiply :: Matrix -> Matrix -> Matrix
multiply (!xa,!xb,!xc,!xd) (!ya,!yb,!yc,!yd) = (
        modulo (xa*ya + xb*yc),
        modulo (xa*yb + xb*yd),
        modulo (xc*ya + xd*yc),
        modulo (xc*yb + xd*yd)
    )

modulo :: Int -> Int
modulo = (`mod` (10^9))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
