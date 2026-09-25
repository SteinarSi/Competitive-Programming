{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [w,h] <- C.getLine <&> (C.words >>> map readInt)
    [x,y] <- C.getLine <&> (C.words >>> map readInt)
    xs <- C.getContents <&> (C.lines >>> drop 1 >>> map (C.readInt >>> fromJust))
    solve (map (id &&& id) [1..min w h]) [] (x,y) xs
        & reverse
        & map (\(x,y) -> show x <> " " <> show y)
        & unlines
        & putStr

solve :: [(Int,Int)] -> [(Int,Int)] -> (Int,Int) -> [(Int,C.ByteString)] -> [(Int,Int)]
solve [] ret _ _  = ret
solve _  ret _ [] = ret
solve dirty ret (x,y) ((d,dir):xs) = solve dirty' ((x',y'):ret) (x',y') xs
  where
    dirty' = filter (\(w,h) -> min x x' > w || w > max x x' || min y y' > h || h > max y y') dirty
    (x',y') = case dir of
        " up"    -> (x,y+d)
        " down"  -> (x,y-d)
        " left"  -> (x-d,y)
        " right" -> (x+d,y)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
