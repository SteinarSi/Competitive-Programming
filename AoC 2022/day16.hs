{-# LANGUAGE TupleSections, FlexibleInstances #-}

import Data.Int (Int8)

import Data.Array (Array, array, (!), assocs, bounds, indices)
import qualified Data.IntSet as S
import qualified Data.Set as SS
import Data.List (sort, nub)
import Data.Bifunctor (bimap, first)
import Data.Maybe (fromJust)

import Debug.Trace (trace)

-- mellom 2413 og 1961

main :: IO ()
main = do
    (g,f) <- fmap (first clique . parse SS.empty [] [] . map words . lines) (readFile "day16-input.txt")
    let valves = S.fromList (filter (\u -> f ! u > 0) (indices f))
        result1 = fst $ search valves 30 0 0 g f
        (elephant, unopened) = search valves 26 0 0 g f
        (me, s) = search unopened 26 0 0 g f
    --print (valves, unopened, s)
    print (result1, elephant + me)

type Flow = Array Int Int
type CliqueGraph = Array (Int, Int) Int

search :: S.IntSet -> Int -> Int -> Int -> CliqueGraph -> Flow -> (Int, S.IntSet)
search unopened time current u g f | time <= 0 = (0, unopened)
                                   | null toSearch = (current * time, unopened)
                                   | otherwise = maximum searches
    where
          searches = map (\v -> first (current * (g!(u,v)) +) $ search (S.delete v unopened) (time-g!(u,v)) (current + f ! v) v g f) toSearch
          toSearch = filter (\v -> f ! v > 0 && S.member v unopened && g ! (u,v) < time)  [1..n]
          n = snd (bounds f)

clique :: Array Int [Int] -> CliqueGraph
clique g = array ((0,0), (n, n)) $ concatMap (\i -> bfs i (S.singleton i) [i] [((i,i),0)] 2) (indices g)
    where 
        n = snd (bounds g)
        bfs :: Int -> S.IntSet -> [Int] -> [((Int, Int), Int)] -> Int -> [((Int, Int), Int)]
        bfs start visited gen ret dist | null next = ret
                                       | otherwise = bfs start (S.union visited (S.fromList next)) next (map (\v -> ((start,v),dist)) next ++ ret) (dist+1)
            where next = nub $ filter (flip S.notMember visited) (concatMap (g!) gen) 

parse :: SS.Set String -> [(String, [String])] -> [(String, Int)] -> [[String]] -> (Array Int [Int], Flow)
parse names neighbours flows [] = let aix = array (0, fromIntegral (SS.size names)-1)
                                      translate = fromJust . flip lookup (zip (sort $ SS.toList names) [0..])
                                      neighbours' = map (bimap translate (map translate)) neighbours
                                      flows' = map (first translate) flows
                                  in  (aix neighbours', aix flows')
parse names neighbours flows ((_:name:_:_:flow:_:_:_:_:neighs):xs) = parse (SS.insert name names) ((name,ns):neighbours) ((name,f):flows) xs
    where ns = last neighs : map init (init neighs)
          f  = read (init (drop 5 flow))

