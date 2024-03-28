{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM, replicateM_)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    t <- C.getLine <&> readInt
    replicateM_ t $ do
        n <- C.getLine <&> readInt
        replicateM n C.getLine >>= (
                map (C.words >>> (\(a:b:_) -> (a, fromIntegral (readInt b))))
                >>> move (0,0) 0
                >>> dist (0,0)
                >>> round
                >>> print
            )

move :: (Double,Double) -> Int -> [(C.ByteString, Int)] -> (Double, Double)
move (x,y) d []             = (x,y)
move (x,y) d (("lt", a):xs) = move (x,y) ((d - a) `mod` 360) xs
move (x,y) d (("rt", a):xs) = move (x,y) ((d + a) `mod` 360) xs
move (x,y) d (("bk", a):xs) = move (x,y) d (("fd",-a):xs)
move (x,y) d (("fd", a):xs) = move (x + l * cos theta, y + l * sin theta) d xs
    where theta = (fromIntegral d * 2 * pi) / 360
          l = fromIntegral a

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt (abs (x1-x2)**2 + abs (y1-y2)**2)
