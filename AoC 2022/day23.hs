{-# LANGUAGE Strict, TupleSections #-}

import Data.Set (Set, fromList, notMember)
import Data.List (sort, find)
import Data.Bool (bool)

main :: IO ()
main = readFile "day23-input.txt" >>= print . simulate 1 (-1) directions . concatMap (\(y, row) -> map ((,y) . fst) $ filter (('#'==) . snd) (zip [0..] row)) . zip [0..] . lines

simulate :: Int -> Int -> [[(Int, Int)]] -> [(Int, Int)] -> (Int, Int)
simulate i res1 dirs e = let next = collide (sort $ search e (fromList e))
                         in  if e == next
                                 then (bool res1 (bounds next) (res1==(-1)), i)
                                 else simulate (i+1) (bool res1 (bounds next) (i==10)) (tail dirs ++ [head dirs]) next
    where 
          search :: [(Int, Int)] -> Set ((Int, Int)) -> [((Int, Int), (Int, Int))]
          search [] _ = []
          search (pos:xs) view = case find (all (flip notMember view . (pos+++))) dirs of
                _ | all (`notMember` view) [pos+++(x,y) | x <- [-1..1], y <- [-1..1], (x,y)/=(0,0)] -> (pos, pos) : search xs view
                Nothing -> (pos, pos) : search xs view
                Just d -> (pos +++ head d, pos) : search xs view

collide :: [((Int, Int), (Int, Int))] -> [(Int, Int)]
collide [] = []
collide ((newPos, oldPos):xs) | null equals = newPos : collide xs
                              | otherwise = oldPos : map snd equals ++ collide nonEquals
    where (equals, nonEquals) = break ((newPos/=) . fst) xs

bounds :: [(Int, Int)] -> Int
bounds e = (xmax-xmin+1) * (ymax-ymin+1) - length e
    where xmin = minimum (map fst e)
          xmax = maximum (map fst e)
          ymin = minimum (map snd e)
          ymax = maximum (map snd e)

directions :: [[(Int, Int)]]
directions = [[(0,-1),(-1,-1),(1,-1)], [(0,1),(-1,1),(1,1)], [(-1,0),(-1,-1),(-1,1)], [(1,0),(1,-1),(1,1)]]

(+++) :: Num a => (a, a) -> (a, a) -> (a, a)
(+++) (x,y) (u,v) = (x+u, y+v)
