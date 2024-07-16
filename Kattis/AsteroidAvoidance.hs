{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((&&&), (***), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [w,h]:[n,m]:rs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let (asteroids, teleports) = splitAt n rs
            & map (init >>> (head &&& last) &&& last)
                ***
              map (head &&& last)

    teleports
        & map (overlap (w,h)
            >>> flip any asteroids
            >>> bool "DOOMSLUG GO!" "DOOMSLUG STOP!")
        & C.unlines
        & C.putStr

overlap :: (Int,Int) -> (Int,Int) -> ((Int,Int),Int) -> Bool
overlap (w,h) (rx,ry) ((cx,cy),r) = squareDist <= r^2
    where
        squareDist = abs (cx-closestX)^2 + abs (cy-closestY)^2

        closestX = clamp rx (rx+w) cx
        closestY = clamp ry (ry+h) cy

clamp :: Int -> Int -> Int -> Int
clamp lo hi x = max lo (min hi x)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
