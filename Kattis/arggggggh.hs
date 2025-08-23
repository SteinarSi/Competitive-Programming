{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:xy:xs <- C.getContents <&> (C.lines >>> map C.words)
    let [x,y] = map (readInt >>> fromIntegral) xy
    putStrLn (walk xs (x,y))

walk :: [[C.ByteString]] -> (Double,Double) -> String
walk [] (x,y) = show x <> " " <> show y
walk ([i,d']:xs) (x,y) = walk xs $ case i of
    "N"  -> (x,y+d)
    "S"  -> (x,y-d)
    "E"  -> (x+d,y)
    "W"  -> (x-d,y)
    "NE" -> (x+sd,y+sd)
    "NW" -> (x-sd,y+sd)
    "SE" -> (x+sd,y-sd)
    "SW" -> (x-sd,y-sd)
  where
    d = fromIntegral (readInt d')
    sd = d / sqrt 2

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
