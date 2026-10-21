{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           System.IO             (hFlush, stdout)

main :: IO ()
main = do
    [n,m] <- C.getLine <&> (C.words >>> map readInt)
    solve ((1,n),(1,m))

solve :: ((Int,Int),(Int,Int)) -> IO ()
solve ((minX,maxX),(minY,maxY)) = do
    let x = (minX + maxX) `div` 2
        y = (minY + maxY) `div` 2
    putStrLn (show x <> " " <> show y)
    hFlush stdout
    response <- C.getLine
    case response of
        "N"     -> solve ((x,x),(y+1,maxY))
        "NE"    -> solve ((x+1,maxX),(y+1,maxY))
        "E"     -> solve ((x+1,maxX),(y,y))
        "SE"    -> solve ((x+1,maxX),(minY,y-1))
        "S"     -> solve ((x,x),(minY,y-1))
        "SW"    -> solve ((minX,x-1),(minY,y-1))
        "W"     -> solve ((minX,x-1),(y,y))
        "NW"    -> solve ((minX,x-1),(y+1,maxY))
        "Iron!" -> pure ()

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
