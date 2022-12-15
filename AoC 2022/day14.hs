{-# LANGUAGE TupleSections, Strict #-}

import Data.List.Split (splitWhen)
import Data.Maybe (maybe)
import Data.Set (Set, insert, notMember, fromList)

main :: IO ()
main = do
    inn <- fmap (concatMap drawLine . lines) (readFile "day14-input.txt")
    let my = maximum (map snd inn)
    print (simulate (my, fromList inn))
    print (simulate (my+2, fromList (inn ++ map (,my+2) [500-my*2..500+my*2])))

simulate :: (Int, Set (Int, Int)) -> Int
simulate = maybe 1 (succ . simulate) . simul (500,0)
    where simul :: (Int, Int) -> (Int, Set (Int, Int)) -> Maybe (Int, Set (Int, Int))
          simul (x,y) c@(my, s) | y >= my = Nothing
                                | notMember (x,y+1) s = simul (x,y+1) c
                                | notMember (x-1,y+1) s = simul (x-1,y+1) c
                                | notMember (x+1,y+1) s = simul (x+1,y+1) c
                                | (x,y) == (500, 0) = Nothing
                                | otherwise = Just ((my, insert (x,y) s))

drawLine :: String -> [(Int, Int)]
drawLine = drawWall . map (\x -> read ('(':x++")")) . filter (/="->") . words
    where drawWall [] = []
          drawWall [x] = []
          drawWall (x:y:xs) = draw x y ++ drawWall (y:xs)
          draw f@(fx, fy) t@(tx, ty) | f == t = [t]
                                     | otherwise = f : draw (fx + signum (tx-fx), fy + signum (ty-fy)) t
