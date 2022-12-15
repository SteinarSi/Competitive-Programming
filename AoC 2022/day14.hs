{-# LANGUAGE TupleSections #-}

import Data.List.Split (splitWhen)
import Data.Array (Array, array, (!), (//), assocs)
import Data.Maybe (maybe)
import Data.Ix (Ix, range)

import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)

type VerticalSlice = (Int, Int, Int, Int, Array (Int, Int) Bool)

main :: IO ()
main = do
    (fx, fy, tx, ty, arr) <- fmap (verticalSlice . concatMap drawLine . lines) (readFile "day14-input.txt")
    print $ simulate (fx, fy, tx, ty, arr)
    --print $ simulateWithFloor (fx, fy, tx, ty, arr)



simulateWithFloor :: VerticalSlice -> Int
simulateWithFloor (fx, fy, tx, ty, arr) = 
    let v@(_,_,_,_,a) = (fx', fy, tx', ty', defaultArray ((fx',fy),(tx', ty')) False // (assocs arr ++ map ((,True) . (,ty')) [fx'..tx']))
    in  trace (show $ (fx', fy, tx', ty')) $ simulate v
    where fx' = fx-50
          tx' = tx+50
          ty' = ty+2

simulate :: VerticalSlice -> Int
simulate = maybe 0 (succ . simulate) . simul (500, 0)
    where simul :: (Int, Int) -> VerticalSlice -> Maybe VerticalSlice
          simul (x, y) v@(fx, fy, tx, ty, arr) | y >= ty = Nothing
                                               | not (arr ! (x, y+1))   = simul (x, y+1) v
                                               | x <= fx = Nothing
                                               | not (arr ! (x-1, y+1)) = simul (x-1, y+1) v
                                               | x >= tx = Nothing
                                               | not (arr ! (x+1, y+1)) = simul (x+1, y+1) v
                                               | otherwise = Just (fx, fy, tx, ty, arr // [((x,y), True)])

verticalSlice :: [(Int, Int)] -> VerticalSlice
verticalSlice ts = (fx, fy, tx, ty, defaultArray ((fx, fy), (tx, ty)) False // map (,True) ts)
    where xs = map fst ts
          ys = map snd ts
          fx = minimum xs
          fy = 0
          tx = maximum xs
          ty = maximum ys

drawLine :: String -> [(Int, Int)]
drawLine = drawWall . map (\x -> read ('(':x++")")) . filter (/="->") . words
    where drawWall [] = []
          drawWall [x] = []
          drawWall (x:y:xs) = draw x y ++ drawWall (y:xs)
          draw f@(fx, fy) t@(tx, ty) | f == t = [t]
                                     | otherwise = f : draw (fx + signum (tx-fx), fy + signum (ty-fy)) t

defaultArray :: Ix i => (i, i) -> a -> Array i a
defaultArray ix a = array ix (map (,a) (range ix))