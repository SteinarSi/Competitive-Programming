{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         (second, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.IntSet           as S
import           Data.Ix               (range)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> trace S.empty (0,0,0,0) (0,0)
        >>> draw
        >>> putStr
    )

draw :: (S.IntSet, (Int,Int,Int,Int), (Int,Int)) -> String
draw (seen, (xf,yf,xt,yt), (tx,ty)) = range ((yf,xf),(yt,xt))
        & map draw'
        & chunksOf (xt-xf+1)
        & unlines
    where
        draw' (0,0) = 'S'
        draw' (y,x) | (x,y) == (tx,ty) = 'E'
                    | x == xf || x == xt || y == yf || y == yt = '#'
                    | S.member (y*1000 + x) seen = '*'
                    | otherwise = ' '

trace :: S.IntSet -> (Int,Int,Int,Int) -> (Int,Int) -> [C.ByteString] -> (S.IntSet, (Int,Int,Int,Int), (Int,Int))
trace seen (minX,minY,maxX,maxY) (x,y) [] = (seen, (minX-1,minY-1,maxX+1,maxY+1), (x,y))
trace seen (minX,minY,maxX,maxY) (x,y) (p:ps) = trace seen' minMax (x',y') ps
    where
        seen' = S.insert (y' * 1000 + x') seen
        (x',y') = case p of
            "left"  -> (x-1,y)
            "right" -> (x+1,y)
            "up"    -> (x,y-1)
            "down"  -> (x,y+1)
        minMax = (
                min minX x',
                min minY y',
                max maxX x',
                max maxY y'
            )

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
        & second (chunksOf k)
        & uncurry (:)
